{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
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
import Data.Vinyl.Functor (Const(..), Identity(..))
import GHC.TypeLits
import Plowtech.Records.OnpingTagCombined
import Servant.API

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