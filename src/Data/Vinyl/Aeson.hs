{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
module Data.Vinyl.Aeson (recordFromJSON, recordToJSON, KnownSymbols) where

import Data.Aeson
import Data.Aeson.Types
import Data.Proxy
import Data.Vinyl
import Data.Vinyl.Functor
import Data.Vinyl.TypeLevel
import Data.Text (pack)
import GHC.TypeLits
import GHC.Exts (Constraint)

type family KnownSymbols (symbols :: [Symbol]) :: Constraint where
  KnownSymbols '[] = ()
  KnownSymbols (symbol ': symbols) = (KnownSymbol symbol, KnownSymbols symbols)

data KnownSymbolDict (s :: Symbol) where
  KnownSymbolDict :: (KnownSymbol s) => Proxy s -> KnownSymbolDict s

data ProxyDict (c :: * -> Constraint) x where
  ProxyDict :: (c x) => Proxy x -> ProxyDict c x

proxyRecord :: (RecApplicative fields) => Rec (Compose Proxy f) fields
proxyRecord = rpure (Compose Proxy)

fieldProxyRecord :: (RecApplicative fields) => Rec Proxy (fields :: [Symbol])
fieldProxyRecord = rpure Proxy

reifyKnownSymbols :: (KnownSymbols fields) => Rec Proxy (fields :: [Symbol]) -> Rec KnownSymbolDict fields
reifyKnownSymbols rec = case rec of
  RNil -> RNil
  (proxy :& proxyRecord) -> KnownSymbolDict proxy :& reifyKnownSymbols proxyRecord

reifyProxyDict :: (RecAll f fields c) => Rec (Compose Proxy f) fields -> Rec (Compose (ProxyDict c) f) fields 
reifyProxyDict rec = case rec of
  RNil -> RNil
  ((Compose proxy) :& proxyRecord) -> Compose (ProxyDict proxy) :& reifyProxyDict proxyRecord

knownSymbolDictToString :: KnownSymbolDict s -> String
knownSymbolDictToString (KnownSymbolDict proxy) = symbolVal proxy

fieldNameRecord :: (KnownSymbols fields, RecApplicative fields) => Rec (Const String) fields
fieldNameRecord = rmap (Const . knownSymbolDictToString) $ reifyKnownSymbols fieldProxyRecord

fieldParserRecord :: (RecAll f fields FromJSON, RecApplicative fields) => Rec (Compose (ProxyDict FromJSON) f) fields
fieldParserRecord = reifyProxyDict proxyRecord


parseWithProxy :: (FromJSON a) => Proxy a -> Value -> Parser a
parseWithProxy _ value = parseJSON value

-- | Combine parallel records into a record of pairs
rzip :: Rec f fields -> Rec g fields -> Rec (Lift (,) f g) fields
rzip f g = rapply (rmap (\x -> Lift (\y -> Lift (x, y))) f) g

-- | Parse a record from a JSON object, where the fields give object field names and field types give parsers
recordFromJSON :: (RecAll f fields FromJSON, KnownSymbols fields, RecApplicative fields) => Value -> Parser (Rec f fields)
recordFromJSON value = 
    rtraverse
      (\(Lift (Compose proxyDict, Const name)) ->
        case proxyDict of
          (ProxyDict proxy) -> 
            withObject 
              "Records must be encoded as Objects"
              (\object -> object .: pack name >>= parseWithProxy proxy)
              value)
  $ rzip fieldParserRecord fieldNameRecord

-- | Serialize a record to a JSON object, where the fields give object field names and field types give serializers
recordToJSON :: (RecAll f fields ToJSON, KnownSymbols fields, RecApplicative fields) => Rec f fields -> Value
recordToJSON rec = 
    object 
  $ recordToList 
  $ rmap (\(Lift (Compose dict, Const name)) -> 
            case dict of
              Dict x -> Const $ pack name .= toJSON x)
  $ rzip (reifyConstraint (Proxy :: Proxy ToJSON) rec) fieldNameRecord 
