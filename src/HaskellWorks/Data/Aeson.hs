module HaskellWorks.Data.Aeson
    ( objectWithoutNulls
    , readJson
    ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Semigroup   ((<>))
import Data.Text
import Text.Read

objectWithoutNulls :: [Pair] -> Value
objectWithoutNulls = object . Prelude.filter (not . isNull . snd)
  where
    isNull Null = True
    isNull _    = False

readJson :: Read a => String -> String -> Parser a
readJson t s = case readMaybe s of
  Just a  -> pure a
  Nothing -> fail $ "Could not parse " <> t
