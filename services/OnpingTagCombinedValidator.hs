{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Lens
import Control.Lens.TH
import Control.Monad (mzero)
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Master.Template (checkTemplates)
import Data.Maybe
import Data.Proxy
import Data.Serialize hiding (encode, decode, Get)
import Data.Text (Text)
import Data.Vinyl
import Data.Vinyl.Aeson
import Data.Vinyl.Functor (Const(..), Identity(..))
import GHC.TypeLits
import Network.Wai (Application)
import Network.Wai.Handler.Warp
import Plowtech.Records.OnpingTagCombined
import Servant.API
import Servant.Server
import SimpleStore

-- | A newtype for JSON-encoding OnpingTagCombined records
newtype OnpingTagCombinedJSON = OnpingTagCombinedJSON { _onpingTagCombinedJSON :: OnpingTagCombined }

makeLenses ''OnpingTagCombinedJSON

-- | Fields for the Named record
type family NamedField a (field :: Symbol) where
  NamedField a "name" = Text
  NamedField a "record" = a

-- | Attribute type for the Named record
newtype NamedAttr a (field :: Symbol) = NamedAttr { _namedAttr :: NamedField a field }

makeLenses ''NamedAttr

-- | The Named record
type Named a = Rec (NamedAttr a) '["name", "record"]

-- | A newtype for JSON-encoding Named records
newtype NamedJSON a = NamedJSON { _namedJSON :: Named a }

makeLenses ''NamedJSON

-- | A newtype for JSON-encoding OnpingTagCombinedTemplate records
newtype OnpingTagCombinedTemplateJSON = OnpingTagCombinedTemplateJSON { _onpingTagCombinedTemplateJSON :: OnpingTagCombinedTemplate }

makeLenses ''OnpingTagCombinedTemplateJSON

-- | The Servant API for validation
type OnpingTagCombinedValidatorAPI =
       "templates" :> Get '[JSON] [NamedJSON OnpingTagCombinedTemplateJSON]
  :<|> "templates" :> ReqBody '[JSON] (NamedJSON OnpingTagCombinedTemplateJSON) :> Post '[JSON] ()
  :<|> "templates" :> Capture "name" Text :> Get '[JSON] OnpingTagCombinedTemplateJSON
  :<|> "validate"  :> Capture "name" Text :> ReqBody '[JSON] OnpingTagCombinedJSON :> Post '[JSON] Bool

-- | The proxy for the validation Servant API type
onpingTagCombinedValidatorAPI :: Proxy OnpingTagCombinedValidatorAPI
onpingTagCombinedValidatorAPI = Proxy

-- | The WAI application serving the API
onpingTagCombinedValidator :: SimpleStore [NamedJSON OnpingTagCombinedTemplateJSON] -> Application
onpingTagCombinedValidator store = 
  serve onpingTagCombinedValidatorAPI
        (getTemplates store :<|> postTemplate store :<|> getTemplate store :<|> validate store)

storeMToServant :: StoreM '[] (Either StoreError a) -> EitherT ServantErr IO a
storeMToServant = bimapEitherT storeErrorToServantError id . EitherT . runStoreM
  where
    storeErrorToServantError storeError = ServantErr 500 (show storeError) "" []

retryLock :: StoreM stack (Either StoreError a) -> StoreM stack (Either StoreError a)
retryLock action = action >>= either (\err -> case err of StoreLocked -> retryLock action; _ -> return $ Left err) (return . Right)

getTemplates :: SimpleStore [NamedJSON OnpingTagCombinedTemplateJSON] -> EitherT ServantErr IO [NamedJSON OnpingTagCombinedTemplateJSON]
getTemplates templateStore = storeMToServant $ withReadLock templateStore $ readSimpleStore StoreHere

postTemplate :: SimpleStore [NamedJSON OnpingTagCombinedTemplateJSON] -> NamedJSON OnpingTagCombinedTemplateJSON -> EitherT ServantErr IO () 
postTemplate templateStore namedTemplateJSON = storeMToServant $ retryLock $ withWriteLock templateStore $ do 
  templates <- readSimpleStore StoreHere
  writeSimpleStore StoreHere $ namedTemplateJSON : templates
  return ()

getTemplate :: SimpleStore [NamedJSON OnpingTagCombinedTemplateJSON] -> Text -> EitherT ServantErr IO OnpingTagCombinedTemplateJSON 
getTemplate templateStore templateName = do 
  mTemplate <- storeMToServant $ withReadLock templateStore $ fmap (listToMaybe . filter ((templateName ==) . (^. (namedJSON . rlens (Proxy :: Proxy "name") . namedAttr)))) $ readSimpleStore StoreHere
  case mTemplate of
    Just template -> return $ template ^. (namedJSON . rlens (Proxy :: Proxy "record") . namedAttr)
    Nothing -> left err404
 
validate :: SimpleStore [NamedJSON OnpingTagCombinedTemplateJSON] -> Text -> OnpingTagCombinedJSON -> EitherT ServantErr IO Bool
validate templateStore templateName candidate = do
  mTemplate <- storeMToServant $ withReadLock templateStore $ fmap (listToMaybe . filter ((templateName ==) . (^. (namedJSON . rlens (Proxy :: Proxy "name") . namedAttr)))) $ readSimpleStore StoreHere
  template <- case mTemplate of
    Just template -> return $ template ^. (namedJSON . rlens (Proxy :: Proxy "record") . namedAttr)
    Nothing -> left err404
  return $ and $ recordToList $ checkTemplates (template ^. onpingTagCombinedTemplateJSON) (candidate ^. onpingTagCombinedJSON)
  

instance ToJSON OnpingTagCombinedJSON where
  toJSON = recordToJSON . _onpingTagCombinedJSON

instance FromJSON OnpingTagCombinedJSON where
  parseJSON = (OnpingTagCombinedJSON <$>) . recordFromJSON

instance (ToJSON a) => ToJSON (NamedJSON a) where
  toJSON = recordToJSON . _namedJSON

instance (FromJSON a) => FromJSON (NamedJSON a) where
  parseJSON = (NamedJSON <$>) . recordFromJSON

instance ToJSON OnpingTagCombinedTemplateJSON where
  toJSON = recordToJSON . _onpingTagCombinedTemplateJSON

instance FromJSON OnpingTagCombinedTemplateJSON where
  parseJSON = (OnpingTagCombinedTemplateJSON <$>) . recordFromJSON

instance (ToJSON (NamedField a field)) => ToJSON (NamedAttr a field) where
  toJSON = toJSON . _namedAttr

instance (FromJSON (NamedField a field)) => FromJSON (NamedAttr a field) where
  parseJSON = (NamedAttr <$>) . parseJSON

instance (FromJSON a, ToJSON a) => Serialize (NamedJSON a) where
  put = put . encode
  get = get >>= maybe mzero return . decode

-- | Entry point
main :: IO ()
main = do
  Right store <- attemptOpenDefaultSimpleStore "template-store" [] 
  run 8000 $ onpingTagCombinedValidator store

