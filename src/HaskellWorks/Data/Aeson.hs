module HaskellWorks.Data.Aeson
    ( (.=?)
    , (.=!)
    , OptionalPairs(..)
    , objectWith
    , objectWithoutNulls
    ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Semigroup
import Data.Text

(.=?) :: ToJSON v => Text -> Maybe v -> OptionalPairs
(.=?) a (Just b) = OptionalPairs ((a .= b):)
(.=?) _ Nothing  = OptionalPairs id

(.=!) :: ToJSON v => Text -> v -> OptionalPairs
(.=!) a b = OptionalPairs ((a .= b):)

newtype OptionalPairs = OptionalPairs ([Pair] -> [Pair])

instance Semigroup OptionalPairs

instance Monoid OptionalPairs where
  mappend (OptionalPairs f) (OptionalPairs g) = OptionalPairs (f . g)
  mempty = OptionalPairs id

objectWith :: OptionalPairs -> Value
objectWith (OptionalPairs f) = object (f [])

objectWithoutNulls :: [Pair] -> Value
objectWithoutNulls = object . Prelude.filter (not . isNull . snd)
  where
    isNull Null = True
    isNull _    = False
