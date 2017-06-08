module HaskellWorks.Data.Aeson
    ( objectWithoutNulls
    ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Text

objectWithoutNulls :: [Pair] -> Value
objectWithoutNulls = object . Prelude.filter (not . isNull . snd)
  where
    isNull Null = True
    isNull _    = False
