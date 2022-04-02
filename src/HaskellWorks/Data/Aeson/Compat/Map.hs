{-# LANGUAGE CPP #-}

module HaskellWorks.Data.Aeson.Compat.Map
  ( module JM
  ) where

#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.KeyMap as JM
#else
import Data.HashMap.Strict as JM
#endif
