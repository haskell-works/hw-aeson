{-# LANGUAGE DerivingVia #-}

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

import Text.Read (readMaybe)
import Data.Aeson (pairs, object, KeyValue((.=)), ToJSON(toJSON, toEncoding), Series, Value(Null))
import Data.Aeson.Encoding (Encoding)
import Data.Aeson.Key (Key)
import Data.Aeson.Types (Pair, Parser)
import Data.Monoid (Endo(..))

infixr 7 .?=
infixr 7 .!=

newtype JsonEndo a = JsonEndo
  { unJsonEndo :: [a] -> [a]
  }
  deriving (Semigroup, Monoid) via (Endo [a])

instance KeyValue a => KeyValue (JsonEndo a) where
  k .= v = JsonEndo (k .= v:)

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
(.?=) :: (KeyValue p, ToJSON v, Monoid p) => Key -> Maybe v -> p
(.?=) k mv = case mv of
  Just v -> k .= v
  Nothing -> mempty

-- | Same as '.=', but with lower precedence to work well with lens.
(.!=) :: (KeyValue kv, ToJSON v) => Key -> v -> kv
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
  toJsonKeyValues :: (KeyValue kv, Monoid kv) => a -> [kv]

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
