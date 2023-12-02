{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
#if MIN_VERSION_aeson(2,2,0)
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
#endif
module HaskellWorks.Data.Aeson
    ( JsonEndo(..)
    , WithJsonKeyValues(..)
    , ToJsonKeyValues(..)
    , objectWithoutNulls
    , readJson
    , objectEndo
    , (.?=)
    , (.!=)
    ) where

import Data.Aeson (pairs, object, KeyValue((.=)), ToJSON(toJSON, toEncoding), Series, Value(Null))
import Data.Aeson.Encoding (Encoding)
import Data.Aeson.Types (Pair, Parser, parseEither)
import Data.Monoid (Endo(..))
import HaskellWorks.Data.Aeson.Compat (Key)
import Text.Read (readMaybe)

import qualified Data.Aeson           as J
import qualified Data.Aeson.Types     as J
import qualified Data.ByteString.Lazy as LBS

infixr 7 .?=
infixr 7 .!=

newtype JsonEndo a = JsonEndo
  { unJsonEndo :: [a] -> [a]
  }
  deriving (Semigroup, Monoid) via (Endo [a])

#if MIN_VERSION_aeson(2,2,0)
instance (ToJSON e, KeyValue e a) => KeyValue e (JsonEndo a) where
  explicitToField f k v = JsonEndo (k .= f v :)
#else
instance KeyValue a => KeyValue (JsonEndo a) where
#endif
  k .= v = JsonEndo (k .= v :)

objectWithoutNulls :: [Pair] -> Value
objectWithoutNulls = object . Prelude.filter (not . isNull . snd)
  where
    isNull Null = True
    isNull _    = False

readJson :: Read a => String -> String -> Parser a
readJson t s = case readMaybe s of
  Just a  -> pure a
  Nothing -> fail $ "Could not parse " <> t

-- | Render optional fields as missing in JSON output.
#if MIN_VERSION_aeson(2,2,0)
(.?=) :: (KeyValue e p, ToJSON v, Monoid p) => Key -> Maybe v -> p
#else
(.?=) :: (KeyValue p, ToJSON v, Monoid p) => Key -> Maybe v -> p
#endif
(.?=) k mv = case mv of
  Just v -> k .= v
  Nothing -> mempty

-- | Same as '.=', but with lower precedence to work well with lens.
#if MIN_VERSION_aeson(2,2,0)
(.!=) :: (KeyValue e kv, ToJSON v) => Key -> v -> kv
#else
(.!=) :: (KeyValue kv, ToJSON v) => Key -> v -> kv
#endif
(.!=) = (.=)

-- | Same as 'object' except used in combination with '.?=' and '.!=' instead of '.='.
--
-- For example:
--
-- @
-- 'toJSON' o = 'objectEndo'
--   [ \"mandatory\" '.!=' o '^.' the @\"mandatory\"
--   , \"optional\"  '.?=' o '^.' the @\"optional\"
--   ]
-- @
objectEndo :: [JsonEndo Pair] -> Value
objectEndo es = object $ unJsonEndo (mconcat es) []

-- | Generate key values from a value of a type.  This can be used
-- in conjunction with 'WithJsonKeyValues' to define a 'ToJSON' instance
-- without having to implement both 'toJSON' and 'toEncoding', thereby
-- reducing boilerplate.
--
-- For example:
--
-- @
-- instance ToJsonEncoding MyType where
--   toJsonEncoding sv =
--     [ "my_field" .!= sv ^. #myField
--     ]
-- @
class ToJsonKeyValues a where
#if MIN_VERSION_aeson(2,2,0)
  toJsonKeyValues :: (KeyValue e kv, Monoid kv) => a -> [kv]
#else
  toJsonKeyValues :: (KeyValue kv, Monoid kv) => a -> [kv]
#endif

-- | For use with language extension DerivingVia.  This derivation provides
-- a ToJSON instance that delegates to the ToJsonKeyValues instance.
--
-- For example:
--
-- @
-- newtype MyType = MyType
--   { myField :: Text
--   } deriving J.ToJSON via WithJsonKeyValues MyType
-- @
newtype WithJsonKeyValues a = WithJsonKeyValues
  { unWithJsonKeyValues :: a
  }

instance ToJsonKeyValues a => ToJSON (WithJsonKeyValues a) where
  toJSON = objectEndo . toJsonKeyValues . unWithJsonKeyValues
  toEncoding = pairs . mconcat . toJsonKeyValues . unWithJsonKeyValues

eitherDecodeWith :: (Value -> Parser a) -> LBS.ByteString -> Either String a
eitherDecodeWith f lbs = J.eitherDecode lbs >>= J.parseEither f
