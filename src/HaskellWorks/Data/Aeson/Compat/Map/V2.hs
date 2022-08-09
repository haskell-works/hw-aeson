{-# LANGUAGE CPP #-}

module HaskellWorks.Data.Aeson.Compat.Map.V2
  (
#if MIN_VERSION_aeson(2,0,0)
    KeyMap
  , Key
  , foldlWithKey
  , foldlWithKey'
  , null
  , lookup
  , size
  , member
  , empty
  , singleton
  , insert
  , delete
  , alterF
  , difference
  , union
  , unionWith
  , unionWithKey
  , intersection
  , intersectionWith
  , intersectionWithKey
  , fromList
  , fromListWith
  , toList
  , toAscList
  , elems
  , fromHashMap
  , toHashMap
  , fromHashMapText
  , toHashMapText
  , fromMap
  , toMap
  , fromMapText
  , toMapText
  , map
  , mapWithKey
  , traverseWithKey
  , foldr
  , foldr'
  , foldl
  , foldl'
  , foldMapWithKey
  , foldrWithKey
  , keys
  , filter
  , filterWithKey
  , mapMaybe
  , mapMaybeWithKey
#endif
  ) where

#if MIN_VERSION_aeson(2,0,0)

import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map as M
import qualified HaskellWorks.Data.Aeson.Compat as J

import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Text (Text)
import Prelude hiding (filter, foldl, foldr, lookup, map, null)

type KeyMap v = KM.KeyMap v
type Key = J.Key

foldlWithKey :: (a -> Key -> b -> a) -> a -> KeyMap b -> a
foldlWithKey f a = M.foldlWithKey f a . KM.toMap

foldlWithKey' :: (a -> Key -> b -> a) -> a -> KeyMap b -> a
foldlWithKey' f a = M.foldlWithKey' f a . KM.toMap

null :: KeyMap v -> Bool
null = KM.null

lookup :: Key -> KeyMap v -> Maybe v
lookup = KM.lookup

size :: KeyMap v -> Int
size = KM.size

member :: Key -> KeyMap v -> Bool
member = KM.member

empty :: KeyMap v
empty = KM.empty

singleton :: Key -> v -> KeyMap v
singleton = KM.singleton

insert :: Key -> v -> KeyMap v -> KeyMap v
insert = KM.insert

delete :: Key -> KeyMap v -> KeyMap v
delete = KM.delete

alterF :: Functor f => (Maybe v -> f (Maybe v)) -> Key -> KeyMap v -> f (KeyMap v)
alterF = KM.alterF

difference :: KeyMap v -> KeyMap v' -> KeyMap v
difference = KM.difference

union :: KeyMap v -> KeyMap v -> KeyMap v
union = KM.union

unionWith :: (v -> v -> v) -> KeyMap v -> KeyMap v -> KeyMap v
unionWith = KM.unionWith

unionWithKey :: (Key -> v -> v -> v) -> KeyMap v -> KeyMap v -> KeyMap v
unionWithKey = KM.unionWithKey

intersection :: KeyMap a -> KeyMap b -> KeyMap a
intersection = KM.intersection

intersectionWith :: (a -> b -> c) -> KeyMap a -> KeyMap b -> KeyMap c
intersectionWith = KM.intersectionWith

intersectionWithKey :: (Key -> a -> b -> c) -> KeyMap a -> KeyMap b -> KeyMap c
intersectionWithKey = KM.intersectionWithKey

fromList :: [(Key, v)] -> KeyMap v
fromList = KM.fromList

fromListWith :: (v -> v -> v) -> [(Key, v)] -> KeyMap v
fromListWith = KM.fromListWith

toList :: KeyMap v -> [(Key, v)]
toList = KM.toList

toAscList :: KeyMap v -> [(Key, v)]
toAscList = M.toList . toMap

elems :: KeyMap v -> [v]
elems = KM.elems

fromHashMap :: HashMap Key v -> KeyMap v
fromHashMap = KM.fromHashMap

toHashMap :: KeyMap v -> HashMap Key v
toHashMap = KM.toHashMap

fromHashMapText :: HashMap Text v -> KeyMap v
fromHashMapText = KM.fromHashMapText

toHashMapText :: KeyMap v -> HashMap Text v
toHashMapText = KM.toHashMapText

fromMap :: Map Key v -> KeyMap v
fromMap = KM.fromList . M.toList

toMap :: KeyMap v -> Map Key v
toMap = M.fromList . KM.toList

fromMapText :: Map Text v -> KeyMap v
fromMapText = KM.fromMap . M.mapKeys K.fromText

toMapText :: KeyMap v -> Map Text v
toMapText = M.mapKeys K.toText . KM.toMap

map :: (a -> b) -> KeyMap a -> KeyMap b
map = KM.map

mapWithKey :: (Key -> a -> b) -> KeyMap a -> KeyMap b
mapWithKey f = KM.fromMap . M.mapWithKey f . KM.toMap

traverseWithKey :: Applicative f => (Key -> v1 -> f v2) -> KeyMap v1 -> f (KeyMap v2)
traverseWithKey = KM.traverseWithKey

foldr :: (a -> b -> b) -> b -> KeyMap a -> b
foldr = KM.foldr

foldr' :: (a -> b -> b) -> b -> KeyMap a -> b
foldr' = KM.foldr'

foldl :: (b -> a -> b) -> b -> KeyMap a -> b
foldl = KM.foldl

foldl' :: (b -> a -> b) -> b -> KeyMap a -> b
foldl' = KM.foldl'

foldMapWithKey :: Monoid m => (Key -> a -> m) -> KeyMap a -> m
foldMapWithKey = KM.foldMapWithKey

foldrWithKey :: (Key -> v -> a -> a) -> a -> KeyMap v -> a
foldrWithKey = KM.foldrWithKey

keys :: KeyMap v -> [Key]
keys = KM.keys

filter :: (v -> Bool) -> KeyMap v -> KeyMap v
filter = KM.filter

filterWithKey :: (Key -> v -> Bool) -> KeyMap v -> KeyMap v
filterWithKey = KM.filterWithKey

mapMaybe :: (a -> Maybe b) -> KeyMap a -> KeyMap b
mapMaybe = KM.mapMaybe

mapMaybeWithKey :: (Key -> v -> Maybe u) -> KeyMap v -> KeyMap u
mapMaybeWithKey = KM.mapMaybeWithKey

#endif