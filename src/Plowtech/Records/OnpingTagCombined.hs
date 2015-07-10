{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Plowtech.Records.OnpingTagCombined where

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Lens.TH
import Data.Aeson
import Data.Master.Template
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Vinyl
import Data.Vinyl.Functor (Compose(..))
import GHC.TypeLits


data Location = Location -- For prototyping fun
  deriving (Show, Eq, Ord)

instance ToJSON Location where
  toJSON Location = object [ "location" .= ("location" :: Text) ]

instance FromJSON Location where
  parseJSON val = withObject "Location must be object" (\obj -> (const Location :: Text -> Location) <$> obj .: "location") val


instance (ToJSON (f (g x))) => ToJSON (Compose f g x) where
  toJSON (Compose x) = toJSON x

instance (FromJSON (f (g x))) => FromJSON (Compose f g x) where
  parseJSON = (Compose <$>) . parseJSON

type family OnpingTagCombinedField (field :: Symbol) where
  OnpingTagCombinedField "location_id"        = Maybe Int
  OnpingTagCombinedField "slave_parameter_id" = Maybe Int
  OnpingTagCombinedField "parameter_tag_id"   = Maybe Int
  OnpingTagCombinedField "description"        = Maybe Text
  OnpingTagCombinedField "unit_id"            = Maybe Int
  OnpingTagCombinedField "status_active"      = Maybe Int
  OnpingTagCombinedField "status_writable"    = Maybe Int
  OnpingTagCombinedField "last_update"        = Maybe UTCTime
  OnpingTagCombinedField "result"             = Maybe Text
  OnpingTagCombinedField "validation_code"    = Maybe Text
  OnpingTagCombinedField "permissions"        = Maybe Int
  OnpingTagCombinedField "delete"             = Maybe Int
  OnpingTagCombinedField "companyIdRef"       = Maybe Int
  OnpingTagCombinedField "siteIdRef"          = Maybe Int
  OnpingTagCombinedField "location"           = Maybe Location
  OnpingTagCombinedField "pid"                = Maybe Int


newtype OnpingTagCombinedAttr (field :: Symbol) = OnpingTagCombinedAttr { _onpingTagCombinedAttr :: OnpingTagCombinedField field }
deriving instance (Eq (OnpingTagCombinedField field)) => Eq (OnpingTagCombinedAttr field)
deriving instance (Ord (OnpingTagCombinedField field)) => Ord (OnpingTagCombinedAttr field)
deriving instance (Show (OnpingTagCombinedField field)) => Show (OnpingTagCombinedAttr field)
deriving instance (Bounded (OnpingTagCombinedField field)) => Bounded (OnpingTagCombinedAttr field)

instance (Enum (OnpingTagCombinedField field)) => Enum (OnpingTagCombinedAttr field) where
  fromEnum = fromEnum . _onpingTagCombinedAttr
  toEnum = OnpingTagCombinedAttr . toEnum

instance (ToJSON (OnpingTagCombinedField field)) => ToJSON (OnpingTagCombinedAttr field) where
  toJSON = toJSON . _onpingTagCombinedAttr

instance (FromJSON (OnpingTagCombinedField field)) => FromJSON (OnpingTagCombinedAttr field) where
  parseJSON value = OnpingTagCombinedAttr <$> parseJSON value

onpingTagCombinedAttr :: Iso' (OnpingTagCombinedAttr field) (OnpingTagCombinedField field)
onpingTagCombinedAttr = iso _onpingTagCombinedAttr OnpingTagCombinedAttr

type OnpingTagCombinedFields = [ "location_id"
                               , "slave_parameter_id"
                               , "parameter_tag_id"
                               , "description"
                               , "unit_id"
                               , "status_active"
                               , "status_writable"
                               , "last_update"
                               , "result"
                               , "validation_code"
                               , "permissions"
                               , "delete"
                               , "companyIdRef"
                               , "siteIdRef"
                               , "location"
                               , "pid"
                               ]

type OnpingTagCombined = Rec OnpingTagCombinedAttr OnpingTagCombinedFields

type OnpingTagCombinedTemplate = Rec (TemplatesFor Normalized Disjunction OnpingTagCombinedAttr) OnpingTagCombinedFields


testOnpingTagCombined :: OnpingTagCombined
testOnpingTagCombined = (OnpingTagCombinedAttr $ Just 4)
                    :&  (OnpingTagCombinedAttr Nothing)
                    :&  (OnpingTagCombinedAttr Nothing)
                    :&  (OnpingTagCombinedAttr Nothing)
                    :&  (OnpingTagCombinedAttr Nothing)
                    :&  (OnpingTagCombinedAttr Nothing)
                    :&  (OnpingTagCombinedAttr Nothing)
                    :&  (OnpingTagCombinedAttr Nothing)
                    :&  (OnpingTagCombinedAttr Nothing)
                    :&  (OnpingTagCombinedAttr Nothing)
                    :&  (OnpingTagCombinedAttr Nothing)
                    :&  (OnpingTagCombinedAttr Nothing)
                    :&  (OnpingTagCombinedAttr Nothing)
                    :&  (OnpingTagCombinedAttr Nothing)
                    :&  (OnpingTagCombinedAttr Nothing)
                    :&  (OnpingTagCombinedAttr Nothing) :& RNil

template :: OnpingTagCombinedTemplate
template = (Compose Meh)
        :& (Compose Meh)
        :& (Compose Meh)
        :& (Compose Meh)
        :& (Compose Meh)
        :& (Compose Meh)
        :& (Compose Meh)
        :& (Compose Meh)
        :& (Compose Meh)
        :& (Compose Meh)
        :& (Compose Meh)
        :& (Compose Meh)
        :& (Compose Meh)
        :& (Compose Meh)
        :& (Compose Meh)
        :& (Compose Meh) :& RNil
