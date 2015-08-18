{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Master.Template.Map where

import           Control.Applicative ((<|>))
import           Control.Lens.Iso
import           Control.Lens ((^.))
import           Data.Aeson
import qualified Data.Map as M
import           Data.Map (Map())

data Has a = Has a | DoesNotHave deriving (Eq, Ord, Read, Show)

maybeHas :: Iso' (Maybe a) (Has a)
maybeHas = iso (maybe DoesNotHave Has)
               (\has -> case has of
                          Has x -> Just x
                          DoesNotHave -> Nothing)

has :: b -> (a -> b) -> Has a -> b
has default_ f = maybe default_ f . (^. from maybeHas)

isoToHasIso :: Iso' s a -> Iso' (Has s) (Has a)
isoToHasIso underIso = iso (has DoesNotHave (Has . (^. underIso)))
                           (has DoesNotHave (Has . (^. from underIso)))

isoToMapIso :: Iso' s a -> Iso' (Map k s) (Map k a)
isoToMapIso underIso = iso (M.map (^. underIso)) (M.map (^. from underIso))

-- | Lift an Iso over a Functor
fmapIso :: (Functor f) => Iso' s a -> Iso' (f s) (f a)
fmapIso underIso = iso (fmap (^. underIso)) (fmap (^. from underIso))

fromUnit :: a -> () -> a
fromUnit = const

instance ToJSON a => ToJSON (Has a) where
  toJSON (Has x) = object $ ["has" .= x]
  toJSON DoesNotHave = object $ ["doesnothave" .= ()]

instance FromJSON a => FromJSON (Has a) where
  parseJSON = withObject "Has must be an object"
              (\obj -> (Has <$> obj .: "has") <|> (fromUnit DoesNotHave <$> obj .: "doesnothave" ))

instance Functor Has where
  fmap f = has DoesNotHave (Has . f)

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
