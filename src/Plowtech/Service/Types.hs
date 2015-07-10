{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
module Plowtech.Service.Types where

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
import Data.Vinyl.Lens
import Data.Vinyl.Functor (Const(..), Identity(..), Compose(..))
import GHC.TypeLits
import Plowtech.Records.OnpingTagCombined
import Servant.API
import Data.Vinyl.TypeLevel
import Data.Master.Template


instance (Eq (f (g x))) => Eq (Compose f g x) where
  (Compose a) == (Compose b) = a == b

-- | A newtype for JSON-encoding OnpingTagCombined records
newtype OnpingTagCombinedJSON = OnpingTagCombinedJSON { _onpingTagCombinedJSON :: OnpingTagCombined }

onpingTagCombinedJSON :: Iso' OnpingTagCombinedJSON OnpingTagCombined
onpingTagCombinedJSON = iso _onpingTagCombinedJSON OnpingTagCombinedJSON

-- | Fields for the Named record
type family NamedField a (field :: Symbol) where
  NamedField a "name" = Text
  NamedField a "record" = a

-- | Attribute type for the Named record
newtype NamedAttr a (field :: Symbol) = NamedAttr { _namedAttr :: NamedField a field }
deriving instance (Eq (NamedField a field)) => Eq (NamedAttr a field)

namedAttr :: Iso' (NamedAttr a field) (NamedField a field)
namedAttr = iso _namedAttr NamedAttr

-- | The Named record
type Named a = Rec (NamedAttr a) '["name", "record"]

-- | A newtype for JSON-encoding Named records
newtype NamedJSON a = NamedJSON { _namedJSON :: Named a }
deriving instance (Eq (Named a)) => Eq (NamedJSON a)

namedJSON :: Iso' (NamedJSON a) (Named a)
namedJSON = iso _namedJSON NamedJSON

-- | A newtype for JSON-encoding OnpingTagCombinedTemplate records
newtype OnpingTagCombinedTemplateJSON = OnpingTagCombinedTemplateJSON { _onpingTagCombinedTemplateJSON :: OnpingTagCombinedTemplate }

onpingTagCombinedTemplateJSON :: Iso' OnpingTagCombinedTemplateJSON OnpingTagCombinedTemplate
onpingTagCombinedTemplateJSON = iso _onpingTagCombinedTemplateJSON OnpingTagCombinedTemplateJSON

instance Eq OnpingTagCombinedTemplateJSON where
  (OnpingTagCombinedTemplateJSON a) == (OnpingTagCombinedTemplateJSON b) = (eqReqOn (Proxy :: Proxy "location_id") a b)
                                                                        && (eqReqOn (Proxy :: Proxy "slave_parameter_id") a b)
                                                                        && (eqReqOn (Proxy :: Proxy "parameter_tag_id") a b)
                                                                        && (eqReqOn (Proxy :: Proxy "description") a b)
                                                                        && (eqReqOn (Proxy :: Proxy "unit_id") a b)
                                                                        && (eqReqOn (Proxy :: Proxy "status_active") a b)
                                                                        && (eqReqOn (Proxy :: Proxy "status_writable") a b)
                                                                        && (eqReqOn (Proxy :: Proxy "last_update") a b)
                                                                        && (eqReqOn (Proxy :: Proxy "result") a b)
                                                                        && (eqReqOn (Proxy :: Proxy "validation_code") a b)
                                                                        && (eqReqOn (Proxy :: Proxy "permissions") a b)
                                                                        && (eqReqOn (Proxy :: Proxy "delete") a b)
                                                                        && (eqReqOn (Proxy :: Proxy "companyIdRef") a b)
                                                                        && (eqReqOn (Proxy :: Proxy "siteIdRef") a b)
                                                                        && (eqReqOn (Proxy :: Proxy "location") a b)
                                                                        && (eqReqOn (Proxy :: Proxy "pid") a b)



eqReqOn :: (RElem r OnpingTagCombinedFields (RIndex r OnpingTagCombinedFields), (Eq (OnpingTagCombinedField r))) 
    => sing r 
    -> Rec (TemplatesFor Normalized Disjunction OnpingTagCombinedAttr) OnpingTagCombinedFields 
    -> Rec (TemplatesFor Normalized Disjunction OnpingTagCombinedAttr) OnpingTagCombinedFields -> Bool
eqReqOn rl template1 template2 = (getCompose $ template1 ^. (rlens rl)) == (getCompose $ template2 ^. (rlens rl))


-- | The Servant API for validation
type OnpingTagCombinedValidatorAPI =
       "templates" :> Get '[JSON] [NamedJSON OnpingTagCombinedTemplateJSON]
  :<|> "templates" :> ReqBody '[JSON] (NamedJSON OnpingTagCombinedTemplateJSON) :> Post '[JSON] ()
  :<|> "templates" :> Capture "name" Text :> Get '[JSON] OnpingTagCombinedTemplateJSON
  :<|> "validate"  :> Capture "name" Text :> ReqBody '[JSON] OnpingTagCombinedJSON :> Post '[JSON] Bool
  :<|>  Raw

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

-- | The proxy for the validation Servant API type
onpingTagCombinedValidatorAPI :: Proxy OnpingTagCombinedValidatorAPI
onpingTagCombinedValidatorAPI = Proxy
