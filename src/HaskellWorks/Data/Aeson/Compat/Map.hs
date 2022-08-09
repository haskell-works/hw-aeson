{-# LANGUAGE CPP #-}

module HaskellWorks.Data.Aeson.Compat.Map
  ( module JM
  ) where

#if MIN_VERSION_aeson(2,0,0)
import HaskellWorks.Data.Aeson.Compat.Map.V2 as JM
#else
import HaskellWorks.Data.Aeson.Compat.Map.V1 as JM
#endif
