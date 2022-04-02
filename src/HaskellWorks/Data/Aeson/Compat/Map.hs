{-# LANGUAGE CPP #-}

module HaskellWorks.Data.Aeson.Compat.Map
  ( module JM
  ) where

import Data.Text (Text)

#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.KeyMap as JM
#else
import Data.HashMap.Strict as JM
#endif

#if MIN_VERSION_aeson(2,0,0)
type Map v = JM.KeyMap v
#else
type Map v = JM.HashMap Text v
#endif
