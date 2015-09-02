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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Master.Template.Vinyl
  (
  -- * Data Kinds
    TemplateLevel(..)
  , Normalization(..)
  
  -- * Templating
  , Template(..)
  , TemplatesFor
  , TemplateFix(..)
  , unFixLevel
  , TemplateBox()
  , weakenTemplate
  , compileTemplateBox
  , compileTemplate
  , checkTemplates
  , MaybeComposite(..)
  , CompositeMaybe(..)

  -- * Lenses and prisms
  , templateFixNormalized
  , templateBox
  , templateFixBox
  , fixAnyLevelIso
  , Fixable (..)
  , _Or
  , _And
  , _Gt
  , _Lt
  , _Not
  , _Eq
  , _In
  )
where

import           Control.Applicative ((<$>), (<|>))
import           Control.Monad
import qualified Control.Lens as L
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bits (testBit, setBit, finiteBitSize)
import           Data.Maybe (listToMaybe)
import           Data.Monoid (All(..))
import           Data.Text (Text())
import qualified Data.Text as T
import           Data.Vinyl.Core
import           Data.Vinyl.Functor
import           GHC.TypeLits

-- | Take apart an or
unOr :: Template Normalized Disjunction a -> [TemplateFix Normalized Conjunction a]
unOr (Or fixes) = fixes

-- | Take apart the existential 'FixAny' that permits violation of normalization constraints
unFixAny :: (forall level' . Template Unnormalized level' a -> k) -> TemplateFix Unnormalized level a -> k
unFixAny existentialKont (FixAny constraint) = existentialKont constraint

-- | Take apart the 'FixLevel that prevents violation of normalization constraints
unFixLevel :: TemplateFix Normalized level a -> Template Normalized level a
unFixLevel (FixLevel c) = c


-- | Functor transformer for template records
type TemplatesFor norm level (f :: u -> *) = Compose (Template norm level) f

-- | Check the templates for a record
-- TODO: Have a real validation type
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
checkTemplate x (ComposedOf compose decompose (FixAny t)) = and $ map (flip checkTemplate t) $ decompose x
checkTemplate x (ComposedOf compose decompose (FixLevel t)) = and $ map (flip checkTemplate t) $ decompose x
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

-- Flag for whether a type is normalized or not
data Normalization
  = Unnormalized
  | Normalized

-- When recurring, do we enforce normalization or not?
data TemplateFix (norm :: Normalization) (level :: TemplateLevel) (a :: *) where
  FixAny   :: Template Unnormalized level' a -> TemplateFix Unnormalized level a
  FixLevel :: Template Normalized level a    -> TemplateFix Normalized level a

-- | Isomorphism between a Normalized fix and the contained template
templateFixNormalized :: L.Iso' (TemplateFix Normalized level a) (Template Normalized level a)
templateFixNormalized = L.iso unFixLevel FixLevel

instance (Eq a) => Eq (TemplateFix norm level a) where
  (==) = eqFixHelper

eqFixHelper :: (Eq a) => TemplateFix norm level a -> TemplateFix norm level' a -> Bool
eqFixHelper (FixAny a) (FixAny b) = eqHelper a b
eqFixHelper (FixLevel a) (FixLevel b) = eqHelper a b
eqFixHelper _ _ = False

eqHelper :: (Eq a) => Template norm level' a -> Template norm level a -> Bool
eqHelper Meh Meh = True
eqHelper (Not a) (Not b) = a == b
eqHelper (Eq a) (Eq b) = a == b
eqHelper (Lt a) (Lt b) = a == b
eqHelper (Gt a) (Gt b) = a == b
eqHelper (In a) (In b) = a == b
eqHelper (And a) (And b) = a == b
eqHelper (Or a) (Or b) = a == b
eqHelper (ComposedOf _ _ a) (ComposedOf _ _ b) = False
eqHelper _ _ = False

-- | Templates
data Template (norm :: Normalization) (level :: TemplateLevel) (a :: *) where
  Meh ::                                       Template norm level a
  Not :: TemplateFix norm Atom a          -> Template norm Atom a
  Eq  :: (Eq a)  => a                       -> Template norm Atom a
  Lt  :: (Ord a) => a                       -> Template norm Atom a
  Gt  :: (Ord a) => a                       -> Template norm Atom a
  In  :: (Eq a)  => [a]                     -> Template norm Atom a
  ComposedOf :: (Eq c, Ord c, ToJSON c) => ([c] -> a) -> (a -> [c]) -> TemplateFix norm Disjunction c -> Template norm Atom a
  And :: [TemplateFix norm Atom a]        -> Template norm Conjunction a
  Or  :: [TemplateFix norm Conjunction a] -> Template norm Disjunction a

instance (Eq a) => Eq (Template norm level a) where
  (==) = eqHelper


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
compileTemplate (ComposedOf compose decompose (FixAny t)) = Or [FixLevel $ And [FixLevel $ ComposedOf compose decompose $ FixLevel $ compileTemplate t]]
compileTemplate (And fixes)           = foldr distributeOrsAnd (Or [FixLevel $ And [FixLevel Meh]]) $ map (unFixAny compileTemplate) fixes  
compileTemplate (Or fixes)            = Or $ concatMap (unFixAny $ unOr . compileTemplate) fixes

-- | Push 'Nots' out to leaves
pushDownNots :: (forall level' . Template Unnormalized level' a -> k) -> Template Unnormalized level a -> k
pushDownNots k Meh              = k $ Not $ FixAny Meh
pushDownNots k (Not (FixAny c)) = k c
pushDownNots k (Eq x)           = k $ Not $ FixAny $ Eq x
pushDownNots k (Lt x)           = k $ Not $ FixAny $ Lt x
pushDownNots k (Gt x)           = k $ Not $ FixAny $ Gt x
pushDownNots k (In x)           = k $ Not $ FixAny $ In x
pushDownNots k (ComposedOf compose decompose (FixAny t)) = k $ ComposedOf compose decompose $ FixAny $ Not $ FixAny t
pushDownNots k (And fixes)      = k $ Or $ map (unFixAny $ pushDownNots FixAny) fixes
pushDownNots k (Or fixes)       = k $ And $ map (unFixAny $ pushDownNots FixAny) fixes

-- | Distribute a conjunction of disjunctions of conjunctions to obtain a disjunction of conjunctions
distributeOrsAnd :: Template Normalized Disjunction a -> Template Normalized Disjunction a -> Template Normalized Disjunction a
distributeOrsAnd (Or andsLeft) (Or andsRight) = Or [FixLevel $ andAnds andLeft andRight | (FixLevel andLeft) <- andsLeft, (FixLevel andRight) <- andsRight]
distributeOrsAnd Meh (Or andsRight) = Or andsRight
distributeOrsAnd (Or andsLeft) Meh = Or andsLeft
distributeOrsAnd Meh Meh = Meh

andAnds :: Template Normalized Conjunction a -> Template Normalized Conjunction a -> Template Normalized Conjunction a
andAnds (And leftFixes) (And rightFixes) = And $ leftFixes ++ rightFixes
andAnds Meh (And rightFixes) = And rightFixes
andAnds (And leftFixes) Meh = And leftFixes
andAnds Meh Meh = Meh

weakenTemplate :: Template norm level a -> TemplateBox a
weakenTemplate Meh = TemplateBox Meh
weakenTemplate (Eq x) = TemplateBox $ Eq x
weakenTemplate (Not (FixAny t)) = case weakenTemplate t of
                           TemplateBox t -> TemplateBox $ Not $ FixAny t
weakenTemplate (Not (FixLevel t)) = case weakenTemplate t of
                                     TemplateBox t -> TemplateBox $ Not $ FixAny t
weakenTemplate (Lt x) = TemplateBox $ Lt x
weakenTemplate (Gt x) = TemplateBox $ Gt x
weakenTemplate (In xs) = TemplateBox $ In xs
weakenTemplate (ComposedOf compose decompose (FixAny t)) = case weakenTemplate t of
                                                             TemplateBox t -> TemplateBox $ ComposedOf compose decompose $ FixAny t
weakenTemplate (ComposedOf compose decompose (FixLevel t)) = case weakenTemplate t of
                                                               TemplateBox t -> TemplateBox $ ComposedOf compose decompose $ FixAny t
weakenTemplate (And []) = TemplateBox $ And []
weakenTemplate (And ts@(FixAny _ : _)) = TemplateBox $ And $ ripTemplates (map (unFixAny weakenTemplate) ts) FixAny
weakenTemplate (And ts@(FixLevel _ : _)) = TemplateBox $ And $ ripTemplates (map (weakenTemplate . unFixLevel) ts) FixAny
weakenTemplate (Or []) = TemplateBox $ Or []
weakenTemplate (Or ts@(FixAny _ : _)) = TemplateBox $ Or $ ripTemplates (map (unFixAny weakenTemplate) ts) FixAny
weakenTemplate (Or ts@(FixLevel _ : _)) = TemplateBox $ Or $ ripTemplates (map (weakenTemplate . unFixLevel) ts) FixAny


compileTemplateBox :: TemplateBox a -> Template Normalized Disjunction a
compileTemplateBox (TemplateBox t) = compileTemplate t

instance (ToJSON a) => ToJSON (Template Normalized Disjunction a) where
  toJSON = toJSON . weakenTemplate

instance (ToJSON a, FromJSON a, Eq a, Ord a, MaybeComposite a) => FromJSON (Template Normalized Disjunction a) where
  parseJSON = (compileTemplateBox <$>) . parseJSON

data TemplateBox a = forall level. TemplateBox { _templateBox :: Template Unnormalized level a }

-- | Isomorphism between a TemplateBox (used for encoding) and a TemplateFix Unnormalized (which can be deconstructed with prisms)
templateBox :: L.Iso' (TemplateBox a) (TemplateFix Unnormalized level a)
templateBox = L.iso (\(TemplateBox template) -> FixAny template) (\(FixAny template) -> TemplateBox template)

data TemplateFixBox a = forall level. TemplateFixBox { _templateFixBox :: TemplateFix Unnormalized level a}

templateFixBox :: L.Iso' (TemplateFixBox a) (TemplateFix Unnormalized level a)
templateFixBox = L.iso (\(TemplateFixBox (FixAny template)) -> FixAny template) TemplateFixBox

instance (ToJSON a) => ToJSON (TemplateFixBox a) where
  toJSON (TemplateFixBox (FixAny template)) = toJSON $ TemplateBox template

instance (ToJSON a) => ToJSON (TemplateBox a) where
  toJSON (TemplateBox Meh) = object ["Meh" .= ()]
  toJSON (TemplateBox (Eq val)) = object ["Eq" .= val]
  toJSON (TemplateBox (Not t)) = object ["Not" .= TemplateFixBox t]
  toJSON (TemplateBox (Lt val)) = object ["Lt" .= val]
  toJSON (TemplateBox (Gt val)) = object ["Gt" .= val]
  toJSON (TemplateBox (In xs)) = object ["In" .= xs]
  toJSON (TemplateBox (ComposedOf _ _ t)) = object ["ComposedOf" .= TemplateFixBox t]
  toJSON (TemplateBox (And ts)) = object ["And" .= fmap TemplateFixBox ts]
  toJSON (TemplateBox (Or ts)) = object ["Or" .= fmap TemplateFixBox ts]

instance (ToJSON a, FromJSON a, Eq a, Ord a, MaybeComposite a) => FromJSON (TemplateBox a) where
  parseJSON o = parseEq o <|> parseNot o <|> parseLt o
            <|> parseGt o <|> parseIn o <|> parseAnd o
            <|> parseOr o <|> parseMeh o <|> parseComposedOf o

parseEq, parseNot, parseLt, parseGt, 
  parseIn, parseAnd, parseOr, parseMeh, parseComposedOf :: forall a . (FromJSON a, ToJSON a, Ord a, Eq a, MaybeComposite a) => Value -> Parser (TemplateBox a)
parseEq (Object o) = do
  value <- (o .: "Eq")
  return $ TemplateBox $ Eq value
parseEq _ = mzero
parseNot (Object o) = do
  (TemplateBox template) <- o .: "Not"
  return $ TemplateBox $ Not $ FixAny $ template
parseNot _ = mzero
parseLt (Object o) = do
  value <- o .: "Lt"
  return $ TemplateBox $ Lt value
parseLt _ = mzero
parseGt (Object o) = do
  value <- o .: "Gt"
  return $ TemplateBox $ Gt value
parseGt _ = mzero
parseIn (Object o) = do
  xs <- o .: "In"
  return $ TemplateBox $ In xs
parseIn _ = mzero
parseAnd (Object o) = do
  xs <- o .: "And"
  return $ TemplateBox $ And $ ripTemplates xs FixAny
parseAnd _ = mzero
parseOr (Object o) = do
  xs <- o .: "Or"
  return $ TemplateBox $ Or $ ripTemplates xs FixAny
parseOr _ = mzero
parseMeh (Object o) = do
  () <- o .: "Meh"
  return $ TemplateBox $ Meh
parseMeh _ = mzero
parseComposedOf (Object o) 
  = case (maybeComposite :: CompositeMaybe a (HasComposite a)) of
      CompositeNothing -> mzero
      CompositeJust fromComposite toComposite ->
        flip fmap (o .: "ComposedOf")
        (\tb -> case tb of
          (TemplateBox t) -> TemplateBox $ ComposedOf fromComposite toComposite $ FixAny t)
parseComposedOf _ = mzero


ripTemplates :: [TemplateBox a] -> (forall level. Template Unnormalized level a -> k) -> [k]
ripTemplates [] _ = []
ripTemplates ((TemplateBox t):ts) f = (f t):(ripTemplates ts f)

-- | Necessary for the reconstruction side of prisms into TemplateBox.
-- Simply propagate this constraint when using the prisms parametrically in the normalization variable.
-- Instances are given for both cases.
class Fixable (normalization :: Normalization) where
  fixFunction :: Template normalization level a -> TemplateFix normalization level a

instance Fixable Normalized where
  fixFunction = FixLevel

instance Fixable Unnormalized where
  fixFunction = FixAny

data CompositeMaybe a (b :: Maybe *) where
  CompositeJust :: (Eq b, Ord b, MaybeComposite b, ToJSON b, FromJSON b) => ([b] -> a) -> (a -> [b]) -> CompositeMaybe a (Just b)
  CompositeNothing :: CompositeMaybe a b

class MaybeComposite a where
  type HasComposite a :: Maybe *
  maybeComposite :: CompositeMaybe a (HasComposite a)

instance MaybeComposite Text where
  type HasComposite Text = Just Char
  maybeComposite = CompositeJust T.pack T.unpack

instance MaybeComposite Char where
  type HasComposite Char = Nothing
  maybeComposite = CompositeNothing

instance MaybeComposite Word where
  type HasComposite Word = Just Bool
  maybeComposite = CompositeJust fromBits toBits
    where
      bitPositions = [0..finiteBitSize (0 :: Word) - 1]
      toBits :: Word -> [Bool]
      toBits word = reverse $ dropWhile not $ reverse $ map (testBit word) bitPositions
      fromBits :: [Bool] -> Word
      fromBits bits = foldr (\(bit, bitPosition) -> if bit then flip setBit bitPosition else id) 0 (zip bits bitPositions)

instance MaybeComposite Bool where
  type HasComposite Bool = Nothing
  maybeComposite = CompositeNothing

-- | Isomorphism between TemplateFix unnormalized at differing levels. Use when applying a prism to Unnormalized boxes.
fixAnyLevelIso :: L.Iso' (TemplateFix Unnormalized level1 a) (TemplateFix Unnormalized level2 a)
fixAnyLevelIso = L.iso castFixAny castFixAny
  where
    castFixAny :: TemplateFix Unnormalized level1 a -> TemplateFix Unnormalized level2 a
    castFixAny (FixAny template) = FixAny template

-- | Prism for getting/setting disjunctive constraints
_Or :: (Fixable normalization) => L.Prism' (TemplateFix normalization Disjunction a) [TemplateFix normalization Conjunction a]
_Or = L.prism' (fixFunction . Or) ripOr
  where ripOr :: TemplateFix normalization level a -> Maybe [TemplateFix normalization Conjunction a]
        ripOr (FixAny (Or xs)) = Just xs
        ripOr (FixLevel (Or xs)) = Just xs
        ripOr _ = Nothing

-- | Prism for getting/setting conjunctive constraints
_And :: (Fixable normalization) => L.Prism' (TemplateFix normalization Conjunction a) [TemplateFix normalization Atom a]
_And = L.prism' (fixFunction . And) ripAnd
  where ripAnd :: TemplateFix normalization level a -> Maybe  [TemplateFix normalization Atom a]
        ripAnd (FixAny (And xs)) = Just xs
        ripAnd (FixLevel (And xs)) = Just xs
        ripAnd _ = Nothing


-- | Prism for getting/setting inverted constraints
_Not :: (Fixable normalization) => L.Prism' (TemplateFix normalization Atom a) (TemplateFix normalization Atom a)
_Not = L.prism' (fixFunction . Not) ripNot
  where ripNot :: (TemplateFix normalization level a) -> Maybe (TemplateFix normalization Atom a)
        ripNot (FixAny (Not t)) = Just t
        ripNot (FixLevel (Not t)) = Just t
        ripNot _ = Nothing

-- | Prism for getting/setting equality constraints
_Eq :: (Fixable normalization, Eq a) => L.Prism' (TemplateFix normalization Atom a) a
_Eq = L.prism' (fixFunction . Eq) ripEq
  where ripEq :: (TemplateFix normalization level a) -> Maybe a
        ripEq (FixAny (Eq v)) = Just v
        ripEq (FixLevel (Eq v)) = Just v
        ripEq _ = Nothing

-- | Prism for getting/setting less-than constraints
_Lt :: (Fixable normalization, Ord a) => L.Prism' (TemplateFix normalization Atom a) a
_Lt = L.prism' (fixFunction . Lt) ripLt
  where ripLt :: (TemplateFix normalization level a) -> Maybe a
        ripLt (FixAny (Lt v)) = Just v
        ripLt (FixLevel (Lt v)) = Just v
        ripLt _ = Nothing

-- | Prism for getting/setting greater-than constraints
_Gt :: (Fixable normalization, Ord a) => L.Prism' (TemplateFix normalization Atom a) a
_Gt = L.prism' (fixFunction . Lt) ripGt
  where ripGt :: (TemplateFix normalization level a) -> Maybe a
        ripGt (FixAny (Gt v)) = Just v
        ripGt (FixLevel (Gt v)) = Just v
        ripGt _ = Nothing

-- | Prism for getting/setting set membership constraints
_In :: (Fixable normalization, Eq a) => L.Prism' (TemplateFix normalization Atom a) [a]
_In = L.prism' (fixFunction . In) ripIn
  where ripIn :: (TemplateFix normalization level a) -> Maybe [a]
        ripIn (FixAny (In xs)) = Just xs
        ripIn (FixLevel (In xs)) = Just xs
        ripIn _ = Nothing
