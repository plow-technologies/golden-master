{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Test where

import Data.Master.Template
import Data.Text
import Data.Aeson
import GHC.Generics
import Control.Monad

data OTCTemplate = OTCTemplate {
  otc :: Template Unnormalized Disjunction Int
, name :: Template Unnormalized Disjunction Text
} deriving (Generic)


instance ToJSON OTCTemplate where
  toJSON (OTCTemplate o n) = object ["otc" .= TemplateBox o, "name" .= TemplateBox n]
instance FromJSON OTCTemplate where
  parseJSON (Object obj) = do
    (TemplateBox o) <- obj .: "otc"
    (TemplateBox nameTemplate) <- obj .: "name"
    return $ OTCTemplate (Or [FixAny o]) (Or [FixAny nameTemplate])
  parseJSON _ = mzero

data OTC = OTC {
  otcTest :: Int
, nameTest :: Text 
}

testOTCTemplate :: OTCTemplate
testOTCTemplate = OTCTemplate o n
  where o = Or [FixAny $ And [FixAny $ Lt 100, FixAny $ Gt 10], FixAny $ Not $ FixAny $ Not $ FixAny $ Eq 150 ]
        n = Or [FixAny $ Eq "Test OTC"]


checkOTCTemplate :: OTC -> OTCTemplate -> Bool
checkOTCTemplate (OTC i n) (OTCTemplate otcTemplate nTemplate) = (checkTemplate i otcTemplate) && checkTemplate n nTemplate


testTemplateBox :: TemplateBox a -> a -> Bool
testTemplateBox (TemplateBox t) val = checkTemplate val t

exampleOTC :: OTC
exampleOTC = OTC 150 "Test OTC"