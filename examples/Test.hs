{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE OverloadedStrings #-}

module Test where

import Data.Master.Template
import Data.Text

data OTCTemplate = OTCTemplate {
  otc :: Template Unnormalized Disjunction Int
, name :: Template Unnormalized Disjunction Text
}

data OTC = OTC {
  otcTest :: Int
, nameTest :: Text 
}

testOTCTemplate :: OTCTemplate
testOTCTemplate = OTCTemplate o n
  where o = Or [FixAny $ And [FixAny $ Lt 100, FixAny $ Gt 10]]
        n = Or [FixAny $ Eq "Test OTC"]


checkOTCTemplate :: OTC -> OTCTemplate -> Bool
checkOTCTemplate (OTC i n) (OTCTemplate otcTemplate nTemplate) = (checkTemplate i otcTemplate) && checkTemplate n nTemplate


exampleOTC :: OTC
exampleOTC = OTC 75 "Test OTC"