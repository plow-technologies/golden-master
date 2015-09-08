module Data.BigEnum (BigEnum(..)) where

import Data.List
import Data.Word

integerFromTo :: Integer -> Integer -> [Integer]
integerFromTo x y = unfoldr (\i -> if i > y then Nothing else Just (i, i + 1)) x

class (Ord a) => BigEnum a where
  toBigEnum :: Integer -> a
  fromBigEnum :: a -> Integer
  bigSucc :: a -> a
  bigSucc x = toBigEnum $ fromBigEnum x + 1
  bigPred :: a -> a
  bigPred x = toBigEnum $ fromBigEnum x - 1
  bigEnumFromTo :: a -> a -> [a]
  bigEnumFromTo x y = map toBigEnum $ integerFromTo (fromBigEnum x) (fromBigEnum y)

instance BigEnum Integer where
  toBigEnum = id
  fromBigEnum = id
  bigEnumFromTo = integerFromTo

instance BigEnum Word where
  toBigEnum = fromIntegral
  fromBigEnum = fromIntegral

