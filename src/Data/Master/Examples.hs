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

module Data.Master.Examples (generateExamples, generateRecordExamples) where

import Data.List
import Data.Master.Template
import Data.Vinyl.Core
import Data.Vinyl.Functor
import Data.Vinyl.TypeLevel

-- | Generate the list of examples for a single template
generateExamples :: (Eq a, Bounded a, Enum a) => Template Normalized level a -> [a]
generateExamples Meh = allExamples
generateExamples (Eq x) = [x]
generateExamples (Lt x) = case reverse $ enumFromTo minBound x of
                       [] -> []
                       (_ : examples) -> reverse examples
generateExamples (Gt x) = case enumFromTo x maxBound of
                       [] -> []
                       (_ : examples) -> examples
generateExamples (In xs) = xs
generateExamples (Not (FixLevel template)) = allExamples \\ generateExamples template
generateExamples (And templates) = foldr union [] $ map (generateExamples . unFixLevel) templates
generateExamples (Or templates) = foldr intersect [] $ map (generateExamples . unFixLevel) templates

-- | Generate a record with lists of examples for each field
generateRecordExamples :: (RecAll f fields Eq, RecAll f fields Bounded, RecAll f fields Enum) => Rec (TemplatesFor Normalized level f) fields -> Rec (Compose [] f) fields
generateRecordExamples RNil = RNil
generateRecordExamples ((Compose template) :& templateRecord) = (Compose $ generateExamples template) :& generateRecordExamples templateRecord

allExamples :: (Bounded a, Enum a) => [a]
allExamples = enumFromTo minBound maxBound
