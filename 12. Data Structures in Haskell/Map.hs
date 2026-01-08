module Map
  ( Map
  , empty
  , isEmpty
  , insert
  , remove
  , lookup
  , keys
  , toList
  , fromList
  )
where

import Data.Bifunctor (second)
import Data.List (find)
import Prelude hiding (lookup)

newtype Map k v = MkMap [(k, v)]
  deriving Show

empty :: Map k v
empty = MkMap []

isEmpty :: Map k v -> Bool
isEmpty (MkMap xs) = null xs

cmpKvp :: Ord k => (k, v) -> (k, v) -> Bool
cmpKvp (key1, _) (key2, _) = key1 < key2

insert :: Ord k => k -> v -> Map k v -> Map k v
insert key value (MkMap xs) =
  -- малко по-четимо е от това на семинара
  MkMap
    $ let (less, geq) = span (`cmpKvp` (key, value)) xs
       in if not (null geq) && fst (head geq) == key
            -- можем и да изкараме less ++ [(key,value)] най-отпред,
            -- но не е кой знае колко по-четимо
            then less ++ [(key, value)] ++ tail geq
            else less ++ [(key, value)] ++ geq

remove :: Ord k => k -> Map k v -> Map k v
remove key (MkMap xs) = MkMap $ filter ((/= key) . fst) xs

lookup :: Ord k => k -> Map k v -> Maybe v
lookup key (MkMap xs) =
  case find ((== key) . fst) xs of
    Nothing -> Nothing
    Just (_, value) -> Just value

keys :: Map k v -> [k]
keys (MkMap xs) = map fst xs

-- >>> insert 5 'd' $ insert 4 'c' $ insert 3 'b' $ insert 4 'a' empty
-- MkMap [(3,'b'),(4,'c'),(5,'d')]

fromList :: Ord k => [(k, v)] -> Map k v
fromList = foldr (uncurry insert) empty

toList :: Ord k => Map k v -> [(k, v)]
toList (MkMap kvpList) = kvpList

-- инстанциите не се експортват

instance Functor (Map k) where
  fmap :: (v -> v') -> Map k v -> Map k v'
  fmap f (MkMap kvpList) = MkMap $ map (second f) kvpList

-- >>> second (+20) ("abcd",10)
-- ("abcd",30)

instance Foldable (Map k) where
  foldr :: (v -> v' -> v') -> v' -> Map k v -> v'
  foldr op nv (MkMap kvpList) = foldr op nv $ map snd kvpList
