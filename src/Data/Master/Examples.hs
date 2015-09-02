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
-- For mapping constraints over things
{-# LANGUAGE ConstraintKinds #-}
-- For RecAll constraints
{-# LANGUAGE UndecidableInstances #-}
-- For passing type params around explicitly
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Master.Examples (Generatable(..)) where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Master.Template.Class
import qualified Data.Master.Template.Vinyl as T (Template)
import           Data.Master.Template.Vinyl hiding (Template)
import           Data.Master.Template.Map
import           Data.Maybe (listToMaybe, maybeToList)
import           Data.Ord (Down(..))
import           Data.Proxy (Proxy(..))
import           Data.Vinyl.Core
import           Data.Vinyl.Functor
import           Data.Vinyl.TypeLevel

class (Checkable a) => Generatable a where
  type Examples a
  generateExamples :: proxy a -> Template a -> Examples a

instance (RecAll f fields Eq, RecAll f fields Ord, RecAll f fields Enum) => Generatable (Rec f fields) where
  type Examples (Rec f fields) = Rec (Compose [] f) fields
  generateExamples = const generateRecordExamples

instance (Ord k, Generatable v) => Generatable (Map k v) where
  type Examples (Map k v) = Map k (Has (Examples v))
  generateExamples = const $ Map.map $ fmap $ generateExamples (Proxy :: Proxy v)

data Bound a = Unbounded | Exclusive a | Inclusive a

data Bounds a
  = Bounds
      { _upperBound :: Bound a
      , _lowerBound :: Bound a
      }
  | UnionBounds { _boundsUnion :: [Bounds a] }

-- | pick from arguments based on an Ordering
withOrdering :: Ordering -> a -> a -> a -> a
withOrdering LT x _ _ = x
withOrdering EQ _ x _ = x
withOrdering GT _ _ x = x

-- | 
intersectBound :: (a -> a -> Ordering) -> Bound a -> Bound a -> Bound a
intersectBound _ Unbounded bound = bound
intersectBound _ bound Unbounded = bound
intersectBound chooseBound (Inclusive a) (Inclusive b) = withOrdering (chooseBound a b) (Inclusive a) (Inclusive a) (Inclusive b)
intersectBound chooseBound (Exclusive a) (Exclusive b) = withOrdering (chooseBound a b) (Exclusive a) (Exclusive a) (Exclusive b)
intersectBound chooseBound (Exclusive a) (Inclusive b) = withOrdering (chooseBound a b) (Exclusive a) (Exclusive a) (Inclusive b)
intersectBound chooseBound (Inclusive a) (Exclusive b) = withOrdering (chooseBound a b) (Inclusive a) (Exclusive b) (Exclusive b)

intersectBounds :: (Ord a) => Bounds a -> Bounds a -> Bounds a
intersectBounds (Bounds upper1 lower1) (Bounds upper2 lower2) = Bounds (intersectBound compare upper1 upper2) (intersectBound (\lower1 lower2 -> compare (Down lower1) (Down lower2)) lower1 lower2)
intersectBounds (UnionBounds bounds1) bounds2 = UnionBounds $ map (intersectBounds bounds2) bounds1
intersectBounds bounds1 (UnionBounds bounds2) = UnionBounds $ map (intersectBounds bounds1) bounds2

negateBounds :: (Ord a) => Bounds a -> Bounds a
negateBounds (UnionBounds bounds) = foldr intersectBounds (Bounds Unbounded Unbounded) $ map negateBounds bounds
negateBounds (Bounds Unbounded Unbounded) = UnionBounds []
negateBounds (Bounds upper lower) = Bounds (flipBound lower) (flipBound upper)
  where
    flipBound :: Bound a -> Bound a
    flipBound Unbounded = Unbounded
    flipBound (Inclusive x) = Exclusive x
    flipBound (Exclusive x) = Inclusive x

unionBounds :: Bounds a -> Bounds a -> Bounds a
unionBounds (UnionBounds bounds1) (UnionBounds bounds2) = UnionBounds $ bounds1 ++ bounds2
unionBounds (UnionBounds bounds1) bounds2 = UnionBounds $ bounds2 : bounds1
unionBounds bounds1 (UnionBounds bounds2) = UnionBounds $ bounds1 : bounds2

-- | Find upper and lower bounds for a template example
findTemplateBounds :: (Ord a) => T.Template Normalized level a -> Bounds a
findTemplateBounds Meh = Bounds Unbounded Unbounded
findTemplateBounds (Eq x) = Bounds (Inclusive x) (Inclusive x)
findTemplateBounds (Lt x) = Bounds (Exclusive x) Unbounded
findTemplateBounds (Gt x) = Bounds Unbounded (Exclusive x)
findTemplateBounds (In xs) = UnionBounds $ map (\x -> Bounds (Inclusive x) (Inclusive x)) xs
findTemplateBounds (ComposedOf from to fix) = UnionBounds []
findTemplateBounds (Not (FixLevel template)) = negateBounds $ findTemplateBounds template
findTemplateBounds (And templates) = foldr intersectBounds (Bounds Unbounded Unbounded) $ map (findTemplateBounds . unFixLevel) templates
findTemplateBounds (Or templates) = foldr unionBounds (UnionBounds []) $ map (findTemplateBounds . unFixLevel) templates

exclude :: (a -> a) -> Bound a -> Maybe a
exclude _ Unbounded     = Nothing
exclude f (Exclusive x) = Just $ f x
exclude _ (Inclusive x) = Just $ x

generateBoundsExamples :: (Eq a, Ord a, Enum a) => Bounds a -> [a]
generateBoundsExamples (Bounds upperBound lowerBound) = 
  let
    lower = exclude succ lowerBound
    upper = exclude pred lowerBound
  in maybe (maybe [toEnum 0] (:[]) lower) (\upperBound -> maybe [upperBound] (\lowerBound -> enumFromTo lowerBound upperBound) lower) upper
generateBoundsExamples (UnionBounds bounds) = foldr union [] $ map generateBoundsExamples bounds

-- | Constraint: Ord and Enum instance are compatible
-- This will not generate all examples. In particular, in cases where a template leaves
-- a range singly or doubly unbounded it will generate only one example
generateTemplateExamples :: (Eq a, Ord a, Enum a) => T.Template Normalized level a -> [a]
generateTemplateExamples = generateBoundsExamples . findTemplateBounds

-- | Generate a record with lists of examples for each field
generateRecordExamples :: (RecAll f fields Eq, RecAll f fields Ord, RecAll f fields Enum) => Rec (TemplatesFor Normalized level f) fields -> Rec (Compose [] f) fields
generateRecordExamples RNil = RNil
generateRecordExamples ((Compose template) :& templateRecord) = (Compose $ generateTemplateExamples template) :& generateRecordExamples templateRecord

