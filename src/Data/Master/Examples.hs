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

module Data.Master.Examples (generateExamples, generateRecordExamples) where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Master.Template.Class
import qualified Data.Master.Template.Vinyl as T (Template)
import           Data.Master.Template.Vinyl hiding (Template)
import           Data.Master.Template.Map
import           Data.Proxy (Proxy(..))
import           Data.Vinyl.Core
import           Data.Vinyl.Functor
import           Data.Vinyl.TypeLevel

class (Checkable a) => Generatable a where
  type Examples a
  generateExamples :: proxy a -> Template a -> Examples a

instance (RecAll f fields Eq, RecAll f fields Bounded, RecAll f fields Enum) => Generatable (Rec f fields) where
  type Examples (Rec f fields) = Rec (Compose [] f) fields
  generateExamples = const generateRecordExamples

instance (Ord k, Generatable v) => Generatable (Map k v) where
  type Examples (Map k v) = Map k (Has (Examples v))
  generateExamples = const $ Map.map $ fmap $ generateExamples (Proxy :: Proxy v)
      

-- | Generate the list of examples for a single template
generateTemplateExamples :: (Eq a, Bounded a, Enum a) => T.Template Normalized level a -> [a]
generateTemplateExamples Meh = allExamples
generateTemplateExamples (Eq x) = [x]
generateTemplateExamples (Lt x) = case reverse $ enumFromTo minBound x of
                       [] -> []
                       (_ : examples) -> reverse examples
generateTemplateExamples (Gt x) = case enumFromTo x maxBound of
                       [] -> []
                       (_ : examples) -> examples
generateTemplateExamples (In xs) = xs
generateTemplateExamples (Not (FixLevel template)) = allExamples \\ generateTemplateExamples template
generateTemplateExamples (And templates) = foldr union [] $ map (generateTemplateExamples . unFixLevel) templates
generateTemplateExamples (Or templates) = foldr intersect [] $ map (generateTemplateExamples . unFixLevel) templates

-- | Generate a record with lists of examples for each field
generateRecordExamples :: (RecAll f fields Eq, RecAll f fields Bounded, RecAll f fields Enum) => Rec (TemplatesFor Normalized level f) fields -> Rec (Compose [] f) fields
generateRecordExamples RNil = RNil
generateRecordExamples ((Compose template) :& templateRecord) = (Compose $ generateTemplateExamples template) :& generateRecordExamples templateRecord

allExamples :: (Bounded a, Enum a) => [a]
allExamples = enumFromTo minBound maxBound
