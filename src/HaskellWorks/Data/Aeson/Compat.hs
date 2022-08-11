{-# LANGUAGE CPP #-}

module HaskellWorks.Data.Aeson.Compat
  ( Key
  , textToKey
  , keyToText
  , stringToKey
  , keyToString
  ) where

import Data.Text (Text)
import Data.Text.Short (ShortText)

import qualified Data.Text as T
import qualified Data.Text.Short as ST

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key    as J
#endif

#if MIN_VERSION_aeson(2,0,0)
type Key = J.Key
#else
type Key = Text
#endif

textToKey :: Text -> Key
#if MIN_VERSION_aeson(2,0,0)
textToKey = J.fromText
#else
textToKey = id
#endif

keyToText :: Key -> Text
#if MIN_VERSION_aeson(2,0,0)
keyToText = J.toText
#else
keyToText = id
#endif

stringToKey :: String -> Key
#if MIN_VERSION_aeson(2,0,0)
stringToKey = J.fromString
#else
stringToKey = T.pack
#endif

keyToString :: Key -> String
#if MIN_VERSION_aeson(2,0,0)
keyToString = J.toString
#else
keyToString = T.unpack
#endif


shortTextToKey :: ShortText -> Key
#if MIN_VERSION_aeson(2,0,0)
shortTextToKey = J.fromText . ST.toText
#elif MIN_VERSION_aeson(2,0,2)
shortTextToKey = J.fromShortText
#else
shortTextToKey = ST.toText
#endif

keyToShortText :: Key -> ShortText
#if MIN_VERSION_aeson(2,0,0)
keyToShortText = ST.fromText . J.toText
#elif MIN_VERSION_aeson(2,0,2)
keyToShortText = J.toShortText
#else
keyToShortText = ST.fromText
#endif
