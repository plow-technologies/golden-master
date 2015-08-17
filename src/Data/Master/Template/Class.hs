{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Master.Template.Class where

import           Data.Map (Map)
import qualified Data.Map as Map
import Data.Master.Template.Map
import Data.Master.Template.Vinyl hiding (Template)
import Data.Proxy (Proxy(..))
import Data.Vinyl
import Data.Vinyl.Functor (Const())

class Checkable a where
  type Template a
  type Result a 
  checkTemplate :: Template a -> a -> Result a
  success :: proxy a -> Result a -> Bool

instance (Ord k, Checkable v) => Checkable (Map k v) where
  type Template (Map k v) = Map k (Has (Template v))
  type Result (Map k v) = Map k (MapCheckResult (Template v) v (Result v))
  checkTemplate templateMap candidateMap = 
    Map.mapWithKey (\key template -> case (template, Map.lookup key candidateMap) of
                                       (Has template, Just candidate) -> Found $ checkTemplate template candidate
                                       (Has template, Nothing) -> Missing template
                                       (DoesNotHave, Just candidate) -> Excluded candidate
                                       (DoesNotHave, Nothing) -> Gone) templateMap
  success _ = Map.foldr (&&) True . Map.map success'
    where
      success' :: MapCheckResult (Template v) v (Result v) -> Bool
      success' Gone = True
      success' (Found result) = success (Proxy :: Proxy v)  result
      success' _ = False

instance Checkable (Rec functor fields) where
  type Template (Rec functor fields) = Rec (TemplatesFor Normalized Disjunction functor) fields
  type Result (Rec functor fields) = Rec (Const Bool) fields
  checkTemplate = checkTemplates
  success = const $ and . recordToList
