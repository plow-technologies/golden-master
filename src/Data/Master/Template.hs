-- For everything
{-# LANGUAGE GADTs      #-}
-- For type level flags, lists, strings
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE KindSignatures #-}
-- For Vinyl and universe polymorphism
{-# LANGUAGE PolyKinds #-}
-- For type-level lists
{-# LANGUAGE TypeOperators #-}
-- For unpacking existentials
{-# LANGUAGE RankNTypes #-}
-- For functions to apply to records
{-# LANGUAGE TypeFamilies #-}
-- For Aeson serialization
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Data.Master.Template where

import Data.Aeson
import Data.Aeson.Types
import Control.Monad
import Control.Applicative ((<$>), (<|>))

import Data.Vinyl.Core
import Data.Vinyl.Functor
import GHC.TypeLits

type TemplatesFor norm level (f :: u -> *) = Compose (Template norm level) f

checkTemplates :: Rec (TemplatesFor norm level f) fields -> Rec f fields -> Rec (Const Bool) fields
checkTemplates constraintRec xRec = rmap (\(Compose constraint) -> Lift $ \x -> Const $ checkTemplate x constraint) constraintRec `rapply` xRec

-- | Check whether a constraint is fulfilled
checkTemplate :: a -> Template norm level a -> Bool
checkTemplate _ Meh = True
checkTemplate x (Not (FixAny template)) = not $ checkTemplate x template
checkTemplate x (Not (FixLevel template)) = not $ checkTemplate x template
checkTemplate x (Eq y) = x == y
checkTemplate x (Lt y) = x < y
checkTemplate x (Gt y) = x > y
checkTemplate x (In y) = x `elem` y
checkTemplate x (And []) = True
checkTemplate x (And conjs@(FixAny _ : _)) = and $ map (unFixAny $ checkTemplate x) conjs
checkTemplate x (And conjs@(FixLevel _ : _)) = and $ map (checkTemplate x . unFixLevel) conjs
checkTemplate x (Or []) = False
checkTemplate x (Or disjs@(FixAny _ : _)) = or $ map (unFixAny $ checkTemplate x) disjs
checkTemplate x (Or disjs@(FixLevel _ : _)) = or $ map (checkTemplate x . unFixLevel) disjs

-- | A constraint compiled by normalizing to horn clauses
type CompiledTemplate = Template Normalized Disjunction 

-- | For enforcing normal form
data TemplateLevel
 = Atom
 | Conjunction
 | Disjunction

instance ToJSON TemplateLevel where
  toJSON Atom = String "Atom"
  toJSON Conjunction = String "Conjunction"
  toJSON Disjunction = String "Disjunction"
instance FromJSON TemplateLevel where
  parseJSON (String tLevelStr) 
    | tLevelStr == "Atom" = return Atom 
    | tLevelStr == "Conjunction" = return Conjunction 
    | tLevelStr == "Disjunction" = return Disjunction 
    | otherwise = mzero
  parseJSON _ = mzero

-- Flag for whether a type is normalized or not
data Normalization
  = Unnormalized
  | Normalized

instance ToJSON Normalization where
  toJSON Unnormalized = String "Unnormalized"
  toJSON Normalized = String "Normalized"
instance FromJSON Normalization where
  parseJSON (String normStr) 
    | normStr == "Unnormalized" = return Unnormalized
    | normStr == "Normalized" = return Normalized
    | otherwise = mzero
  parseJSON _ = mzero

-- When recurring, do we enforce normalization or not?
data TemplateFix (norm :: Normalization) (level :: TemplateLevel) (a :: *) where
  FixAny   :: Template Unnormalized level' a -> TemplateFix Unnormalized level a
  FixLevel :: Template Normalized level a    -> TemplateFix Normalized level a

-- | Templates
data Template (norm :: Normalization) (level :: TemplateLevel) (a :: *) where
  Meh ::                                       Template norm level a
  Not :: TemplateFix norm Atom a          -> Template norm Atom a
  Eq  :: (Eq a)  => a                       -> Template norm Atom a
  Lt  :: (Ord a) => a                       -> Template norm Atom a
  Gt  :: (Ord a) => a                       -> Template norm Atom a
  In  :: (Eq a)  => [a]                     -> Template norm Atom a
  And :: [TemplateFix norm Atom a]        -> Template norm Conjunction a
  Or  :: [TemplateFix norm Conjunction a] -> Template norm Disjunction a

-- | Convert a constraint from unnormalized form to normal form
compileTemplate :: Template Unnormalized level a -> Template Normalized Disjunction a
compileTemplate Meh                   = Meh
compileTemplate (Not (FixAny Meh))    = Or [FixLevel $ And [FixLevel $ Not $ FixLevel $ Meh]]
compileTemplate (Not (FixAny (Eq x))) = Or [FixLevel $ And [FixLevel $ Not $ FixLevel $ Eq x]]
compileTemplate (Not (FixAny (Lt x))) = Or [FixLevel $ And [FixLevel $ Not $ FixLevel $ Lt x]]
compileTemplate (Not (FixAny (Gt x))) = Or [FixLevel $ And [FixLevel $ Not $ FixLevel $ Gt x]]
compileTemplate (Not (FixAny (In x))) = Or [FixLevel $ And [FixLevel $ Not $ FixLevel $ In x]]
compileTemplate (Not (FixAny c))      = pushDownNots compileTemplate c
compileTemplate (Eq x)                = Or [FixLevel $ And [FixLevel $ Eq x]]
compileTemplate (Lt x)                = Or [FixLevel $ And [FixLevel $ Lt x]]
compileTemplate (Gt x)                = Or [FixLevel $ And [FixLevel $ Gt x]]
compileTemplate (In x)                = Or [FixLevel $ And [FixLevel $ In x]]
compileTemplate (And fixes)           = foldr distributeOrsAnd (Or []) $ map (unFixAny compileTemplate) fixes  
compileTemplate (Or fixes)            = Or $ concatMap (unFixAny $ unOr . compileTemplate) fixes

-- | Push 'Nots' out to leaves
pushDownNots :: (forall level' . Template Unnormalized level' a -> k) -> Template Unnormalized level a -> k
pushDownNots k Meh              = k $ Not $ FixAny Meh
pushDownNots k (Not (FixAny c)) = k c
pushDownNots k (Eq x)           = k $ Not $ FixAny $ Eq x
pushDownNots k (Lt x)           = k $ Not $ FixAny $ Lt x
pushDownNots k (Gt x)           = k $ Not $ FixAny $ Gt x
pushDownNots k (In x)           = k $ Not $ FixAny $ In x
pushDownNots k (And fixes)      = k $ Or $ map (unFixAny $ pushDownNots FixAny) fixes
pushDownNots k (Or fixes)       = k $ And $ map (unFixAny $ pushDownNots FixAny) fixes

-- | Take apart an or
unOr :: Template Normalized Disjunction a -> [TemplateFix Normalized Conjunction a]
unOr (Or fixes) = fixes

-- | Take apart the existential 'FixAny' that permits violation of normalization constraints
unFixAny :: (forall level' . Template Unnormalized level' a -> k) -> TemplateFix Unnormalized level a -> k
unFixAny existentialKont (FixAny constraint) = existentialKont constraint

-- | Take apart the 'FixLevel that prevents violation of normalization constraints
unFixLevel :: TemplateFix Normalized level a -> Template Normalized level a
unFixLevel (FixLevel c) = c

-- | Distribute a conjunction of disjunctions of conjunctions to obtain a disjunction of conjunctions
distributeOrsAnd :: Template Normalized Disjunction a -> Template Normalized Disjunction a -> Template Normalized Disjunction a
distributeOrsAnd (Or andsLeft) (Or andsRight) = Or [FixLevel $ andAnds andLeft andRight | (FixLevel andLeft) <- andsLeft, (FixLevel andRight) <- andsRight]

andAnds :: Template norm Conjunction a -> Template norm Conjunction a -> Template norm Conjunction a
andAnds (And leftFixes) (And rightFixes) = And $ leftFixes ++ rightFixes
 


data TemplateBox a = forall level. TemplateBox { unTemplateBox :: Template Unnormalized level a }

data TemplateFixBox a = forall level. TemplateFixBox { unTemplateFixBox :: TemplateFix Unnormalized level a}

instance (ToJSON a) => ToJSON (TemplateFixBox a) where
  toJSON (TemplateFixBox (FixAny template)) = toJSON $ TemplateBox template

instance (ToJSON a) => ToJSON (TemplateBox a) where
  toJSON (TemplateBox Meh) = String "Meh"
  toJSON (TemplateBox (Eq val)) = object ["Eq" .= val]
  toJSON (TemplateBox (Not t)) = object ["Not" .= TemplateFixBox t]
  toJSON (TemplateBox (Lt val)) = object ["Lt" .= val]
  toJSON (TemplateBox (Gt val)) = object ["Gt" .= val]
  toJSON (TemplateBox (In xs)) = object ["In" .= xs]
  toJSON (TemplateBox (And ts)) = object ["And" .= fmap TemplateFixBox ts]
  toJSON (TemplateBox (Or ts)) = object ["Or" .= fmap TemplateFixBox ts]

instance (FromJSON a, Eq a, Ord a) => FromJSON (TemplateBox a) where
  parseJSON o = parseEq o <|> parseNot o <|> parseLt o
            <|> parseGt o <|> parseIn o <|> parseAnd o
            <|> parseOr o <|> parseMeh o

parseEq, parseNot, parseLt, parseGt, 
  parseIn, parseAnd, parseOr, parseMeh :: (FromJSON a, Ord a, Eq a) => Value -> Parser (TemplateBox a)
parseEq (Object o) = do
  value <- (o .: "Eq")
  return $ TemplateBox $ Or [FixAny $ Eq value]
parseEq _ = mzero
parseNot (Object o) = do
  (TemplateBox template) <- o .: "Not"
  return $ TemplateBox $ Not $ FixAny $ template
parseNot _ = mzero
parseLt (Object o) = do
  value <- o .: "Lt"
  return $ TemplateBox $ Or [FixAny $ Lt value]
parseLt _ = mzero
parseGt (Object o) = do
  value <- o .: "Gt"
  return $ TemplateBox $ Or [FixAny $ Gt value]
parseGt _ = mzero
parseIn (Object o) = do
  xs <- o .: "In"
  return $ TemplateBox $ Or [FixAny $ In xs]
parseIn _ = mzero
parseAnd (Object o) = do
  xs <- o .: "And"
  return $ TemplateBox $ And $ ripTemplates xs FixAny
parseAnd _ = mzero
parseOr (Object o) = do
  xs <- o .: "Or"
  return $ TemplateBox $ Or $ ripTemplates xs FixAny
parseOr _ = mzero
parseMeh obj
  | obj == (String "Meh") = return $ TemplateBox Meh
  | otherwise = mzero


ripTemplates :: [TemplateBox a] -> (forall level. Template Unnormalized level a -> k) -> [k]
ripTemplates [] _ = []
ripTemplates ((TemplateBox t):ts) f = (f t):(ripTemplates ts f)