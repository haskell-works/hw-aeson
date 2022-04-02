{-# LANGUAGE CPP #-}

module HaskellWorks.Data.Aeson.Compat.Map
  ( module JM
  , KeyMap(..)
  , foldlWithKey'
  ) where

import qualified Data.Map as M
import qualified HaskellWorks.Data.Aeson.Compat as J

import Data.Text (Text)

#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.KeyMap as JM
#else
import qualified Data.HashMap.Strict as JM
import Data.HashMap.Strict hiding (foldlWithKey')
#endif

#if !MIN_VERSION_aeson(2,0,0)
type KeyMap v = JM.HashMap Text v
#endif

foldlWithKey' :: (a -> J.Key -> b -> a) -> a -> KeyMap b -> a
#if MIN_VERSION_aeson(2,0,0)
foldlWithKey' f a = M.foldlWithKey' f a . JM.toMap
#else
foldlWithKey'= JM.foldlWithKey'
#endif

