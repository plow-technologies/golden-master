{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Master.Template.Map where

import           Control.Applicative ((<|>))
import           Control.Lens.Iso
import           Control.Lens ((^.))
import           Data.Aeson
import qualified Data.Map as M
import           Data.Map (Map())

data Has a = Has a | MaybeHas a | DoesNotHave deriving (Eq, Ord, Read, Show)

has :: b -> (a -> b) -> (a -> b) -> Has a -> b
has default_ _ _ DoesNotHave = default_ 
has _ maybeK _ (MaybeHas x) = maybeK x
has _ _ hasK (Has x) = hasK x

isoToHasIso :: Iso' s a -> Iso' (Has s) (Has a)
isoToHasIso underIso = iso (has DoesNotHave (MaybeHas . (^. underIso)) (Has . (^. underIso)))
                           (has DoesNotHave (MaybeHas . (^. from underIso)) (Has . (^. from underIso)))

isoToMapIso :: Iso' s a -> Iso' (Map k s) (Map k a)
isoToMapIso underIso = iso (M.map (^. underIso)) (M.map (^. from underIso))

-- | Lift an Iso over a Functor
fmapIso :: (Functor f) => Iso' s a -> Iso' (f s) (f a)
fmapIso underIso = iso (fmap (^. underIso)) (fmap (^. from underIso))

fromUnit :: a -> () -> a
fromUnit = const

instance ToJSON a => ToJSON (Has a) where
  toJSON (Has x) = object ["has" .= x]
  toJSON (MaybeHas x) = object ["maybehas" .= x]
  toJSON DoesNotHave = object ["doesnothave" .= ()]

instance FromJSON a => FromJSON (Has a) where
  parseJSON = withObject "Has must be an object"
              (\obj -> (Has <$> obj .: "has") <|> (MaybeHas <$> obj .: "maybehas") <|> (fromUnit DoesNotHave <$> obj .: "doesnothave" ))

instance Functor Has where
  fmap f = has DoesNotHave (MaybeHas . f) (Has . f)

data MapCheckResult template a result = Missing template | Excluded a | Found result | Gone deriving (Eq, Ord, Read, Show)

instance (ToJSON template, ToJSON a, ToJSON result) => ToJSON (MapCheckResult template a result) where
  toJSON (Missing template) = object [ "missing" .= template ]
  toJSON (Excluded x) = object [ "excluded" .= x ]
  toJSON (Found result) = object [ "found" .= result ]
  toJSON Gone = object [ "gone" .= () ]

instance (FromJSON template, FromJSON a, FromJSON result) => FromJSON (MapCheckResult template a result) where
  parseJSON = withObject "MapCheckResult must be an object"
              (\obj ->
                    (Missing <$> obj .: "missing")
                <|> (Excluded <$> obj .: "excluded")
                <|> (Found <$> obj .: "found")
                <|> (fromUnit Gone <$> obj .: "gone"))
