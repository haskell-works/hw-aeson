module HaskellWorks.Data.Aeson.Compat.Map.V1
  ( KeyMap
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
  ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
import qualified HaskellWorks.Data.Aeson.Compat as J

import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Text (Text)
import Prelude hiding (filter, foldl, foldr, lookup, map, null)

type KeyMap v = HM.HashMap Text v
type Key = Text

foldlWithKey :: (a -> Key -> b -> a) -> a -> KeyMap b -> a
foldlWithKey = HM.foldlWithKey

foldlWithKey' :: (a -> Key -> b -> a) -> a -> KeyMap b -> a
foldlWithKey' = HM.foldlWithKey'

null :: KeyMap v -> Bool
null = HM.null

lookup :: Key -> KeyMap v -> Maybe v
lookup = HM.lookup

size :: KeyMap v -> Int
size = HM.size

member :: Key -> KeyMap v -> Bool
member = HM.member

empty :: KeyMap v
empty = HM.empty

singleton :: Key -> v -> KeyMap v
singleton = HM.singleton

insert :: Key -> v -> KeyMap v -> KeyMap v
insert = HM.insert

delete :: Key -> KeyMap v -> KeyMap v
delete = HM.delete

alterF :: Functor f => (Maybe v -> f (Maybe v)) -> Key -> KeyMap v -> f (KeyMap v)
alterF = HM.alterF

difference :: KeyMap v -> KeyMap v' -> KeyMap v
difference = HM.difference

union :: KeyMap v -> KeyMap v -> KeyMap v
union = HM.union

unionWith :: (v -> v -> v) -> KeyMap v -> KeyMap v -> KeyMap v
unionWith = HM.unionWith

unionWithKey :: (Key -> v -> v -> v) -> KeyMap v -> KeyMap v -> KeyMap v
unionWithKey = HM.unionWithKey

intersection :: KeyMap a -> KeyMap b -> KeyMap a
intersection = HM.intersection

intersectionWith :: (a -> b -> c) -> KeyMap a -> KeyMap b -> KeyMap c
intersectionWith = HM.intersectionWith

intersectionWithKey :: (Key -> a -> b -> c) -> KeyMap a -> KeyMap b -> KeyMap c
intersectionWithKey = HM.intersectionWithKey

fromList :: [(Key, v)] -> KeyMap v
fromList = HM.fromList

fromListWith :: (v -> v -> v) -> [(Key, v)] -> KeyMap v
fromListWith = HM.fromListWith

toList :: KeyMap v -> [(Key, v)]
toList = HM.toList

toAscList :: KeyMap v -> [(Key, v)]
toAscList = M.toList . toMap

elems :: KeyMap v -> [v]
elems = HM.elems

fromHashMap :: HashMap Key v -> KeyMap v
fromHashMap = id

toHashMap :: KeyMap v -> HashMap Key v
toHashMap = id

fromHashMapText :: HashMap Text v -> KeyMap v
fromHashMapText = id

toHashMapText :: KeyMap v -> HashMap Text v
toHashMapText = id

fromMap :: Map Key v -> KeyMap v
fromMap = HM.fromList . M.toList

toMap :: KeyMap v -> Map Key v
toMap = M.fromList . HM.toList

fromMapText :: Map Text v -> KeyMap v
fromMapText = fromMap

toMapText :: KeyMap v -> Map Text v
toMapText = toMap

map :: (a -> b) -> KeyMap a -> KeyMap b
map = HM.map

mapWithKey :: (Key -> a -> b) -> KeyMap a -> KeyMap b
mapWithKey = HM.mapWithKey

traverseWithKey :: Applicative f => (Key -> v1 -> f v2) -> KeyMap v1 -> f (KeyMap v2)
traverseWithKey = HM.traverseWithKey

foldr :: (a -> b -> b) -> b -> KeyMap a -> b
foldr = HM.foldr

foldr' :: (a -> b -> b) -> b -> KeyMap a -> b
foldr' = HM.foldr'

foldl :: (b -> a -> b) -> b -> KeyMap a -> b
foldl = HM.foldl

foldl' :: (b -> a -> b) -> b -> KeyMap a -> b
foldl' = HM.foldl'

foldMapWithKey :: Monoid m => (Key -> a -> m) -> KeyMap a -> m
foldMapWithKey = HM.foldMapWithKey

foldrWithKey :: (Key -> v -> a -> a) -> a -> KeyMap v -> a
foldrWithKey = HM.foldrWithKey

keys :: KeyMap v -> [Key]
keys = HM.keys

filter :: (v -> Bool) -> KeyMap v -> KeyMap v
filter = HM.filter

filterWithKey :: (Key -> v -> Bool) -> KeyMap v -> KeyMap v
filterWithKey = HM.filterWithKey

mapMaybe :: (a -> Maybe b) -> KeyMap a -> KeyMap b
mapMaybe = HM.mapMaybe

mapMaybeWithKey :: (Key -> v -> Maybe u) -> KeyMap v -> KeyMap u
mapMaybeWithKey = HM.mapMaybeWithKey
