{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Plowtech.Records.OnpingTagCombined where

import Control.Lens.TH
import Data.Master.Template
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Vinyl
import GHC.TypeLits


data Location = Location -- For prototyping fun
  deriving (Show)

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

makeLenses ''OnpingTagCombinedAttr

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