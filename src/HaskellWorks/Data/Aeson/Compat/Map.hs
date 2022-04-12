{-# LANGUAGE CPP #-}

module HaskellWorks.Data.Aeson.Compat.Map
  ( module JM
  , KeyMap(..)
  , foldlWithKey'
  ) where

import qualified Data.Map as M
import qualified HaskellWorks.Data.Aeson.Compat as J

import Data.HashMap.Strict (HashMap)
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

#if MIN_VERSION_aeson(2,0,0)
fromHashMapText :: HashMap Text a ->  KeyMap a
fromHashMapText = JM.fromHashMapText
#else
fromHashMapText :: HashMap Text a -> HashMap Text a
fromHashMapText = id
#endif

#if MIN_VERSION_aeson(2,0,0)
toHashMapText :: KeyMap a -> HashMap Text a
toHashMapText = JM.toHashMapText
#else
toHashMapText :: HashMap Text a -> HashMap Text a
toHashMapText = id
#endif
