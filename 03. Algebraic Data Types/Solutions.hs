module Solutions where

-- така можем да скрием дефинициите от стандартната библиотека,
-- за да не clash-ват с тези, които ние пишем

import Prelude hiding (Maybe (..), Ordering (..), repeat)

repeat :: a -> [a]
repeat x = x : repeat x

-- >>> take 3 (repeat "hi")
-- ["hi","hi","hi"]

from :: Int -> [Int]
from x = x : from (x + 1)

-- >>> take 10 (from 4)
-- [4,5,6,7,8,9,10,11,12,13]

fibs :: [Int]
fibs = fibs' 0 1
  where
    fibs' a b = a : fibs' b (a + b)

-- >>> take 10 fibs
-- [0,1,1,2,3,5,8,13,21,34]

data Ordering = LessThan | Equal | GreaterThan
  deriving (Show, Eq, Ord, Enum)

cmpInt :: Int -> Int -> Ordering
cmpInt x y
  | x < y = LessThan
  | x == y = Equal
  | x > y = GreaterThan

type Sides = Int

type Length = Double

data Shape
  = Triangle Length Length Length
  | Square Length
  | Polygon Sides Length
  deriving (Show, Eq, Ord)

triangle :: Shape
triangle = Triangle 4.5 1 3.6

square :: Shape
square = Square 4

polygon :: Shape
polygon = Polygon 5 6.6

perimeter :: Shape -> Double
perimeter (Square x) = x * 4
perimeter (Triangle x y z) = x + y + z
perimeter (Polygon n x) = fromIntegral n * x

-- >>> perimeter square
-- 16.0

-- >>> perimeter polygon
-- 33.0

numberOfSides :: Shape -> Int
numberOfSides (Square _) = 4
-- record patterns
numberOfSides (Triangle {}) = 3
numberOfSides (Polygon n _) = n

-- >>> numberOfSides triangle
-- 3

prettyPrint :: Shape -> String
prettyPrint s =
  "This figure is a "
    ++ case s of
      Square x -> "square with a side of length " ++ show x
      Triangle x y z -> "triangle with sides " ++ show x ++ ", " ++ show y ++ ", and " ++ show z
      Polygon n x -> "regular polygon that has " ++ show n ++ " sides, each with a length of " ++ show x

data Pair a b = Pair a b
  deriving (Show, Eq, Ord)

myFst :: Pair a b -> a
myFst (Pair x _) = x

mySnd :: Pair a b -> b
mySnd (Pair _ y) = y

myRev :: Pair a b -> Pair b a
myRev (Pair x y) = Pair y x

pairToTuple :: Pair a b -> (a, b)
pairToTuple (Pair x y) = (x, y)

tupleToPair :: (a, b) -> Pair a b
tupleToPair (x, y) = Pair x y

cmp :: (Ord a) => a -> a -> Ordering
cmp x y
  | x < y = LessThan
  | x == y = Equal
  | x > y = GreaterThan

cmpPair :: (Ord a, Ord b) => Pair a b -> Pair a b -> Ordering
cmpPair (Pair x1 y1) (Pair x2 y2) =
  case cmp x1 x2 of
    Equal -> cmp y1 y2
    res -> res

pairsToList :: [Pair a b] -> Pair [a] [b]
pairsToList pairs = helper [] [] pairs
  where
    helper :: [a] -> [b] -> [Pair a b] -> Pair [a] [b]
    helper as bs [] = Pair as bs
    helper as bs ((Pair a b) : xs) =
      helper (as ++ [a]) (bs ++ [b]) xs

-- >>> pairsToList [Pair 1 2, Pair 3 4]
-- Pair [1,3] [2,4]

data Maybe a = Just a | Nothing
  deriving (Show, Eq, Ord)

safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv a b = Just (a / b)

-- >>> safeDiv 5 6
-- Just 0.8333333333333334

-- >>> safeDiv 9 0
-- Nothing

addM :: Maybe Int -> Maybe Int -> Maybe Int
addM (Just x) (Just y) = Just (x + y)
addM _ _ = Nothing

-- >>> addM (Just 1) (Just 6)
-- Just 7

-- >>> addM Nothing (Just 8)
-- Nothing

sumM :: [Maybe Int] -> Maybe Int
sumM [] = Just 0
sumM ((Just x) : xs) =
  case sumM xs of
    Just s -> Just (x + s)
    _ -> Nothing
sumM _ = Nothing

-- >>> sumM [Just 1, Just 2, Just 3, Just 4]
-- Just 10

-- >>> sumM [Just 1, Nothing]
-- Nothing

-- >>> sumM (take 10 (repeat Nothing))
-- Nothing

-- >>> sumM []
-- Just 0

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust _ = error "no value of type a in Nothing"

-- все още не сме говорили за функции от по-висок ред,
-- така че ще напишем следната функция по сравнително тромав начин

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) = quicksort less ++ [x] ++ quicksort greater
  where
    less = filterLessEq x xs
    greater = filterGreater x xs

    filterLessEq :: (Ord a) => a -> [a] -> [a]
    filterLessEq _ [] = []
    filterLessEq x (y : ys)
      | y <= x = y : filterLessEq x ys
      | otherwise = filterLessEq x ys

    filterGreater :: (Ord a) => a -> [a] -> [a]
    filterGreater _ [] = []
    filterGreater x (y : ys)
      | y > x = y : filterGreater x ys
      | otherwise = filterGreater x ys

-- >>> quicksort [5,2,6,1,9,0,-1,3]
-- [-1,0,1,2,3,5,6,9]

-- премахва повторенията. името идва от Data.List.nub
nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x : xs)
  | x `elem` xs = nub xs
  | otherwise = x : nub xs

-- >>> nub [9,9,2,4,1,9,2,5]
-- [4,1,9,2,5]

-- ще държим множеството в сортиран вид,
-- понеже прави проверките по-лесни
newtype Set a = Set [a]
  deriving (Show)

emptySet :: Set a
emptySet = Set []

singletonSet :: a -> Set a
singletonSet x = Set [x]

-- забележете, че ако Ord a, то Eq a,
-- т.е. можем да изпуснем Eq a
setFromList :: (Eq a, Ord a) => [a] -> Set a
setFromList xs = Set (nub (quicksort xs))

setToList :: Set a -> [a]
setToList (Set xs) = xs

set1 :: Set Int
set1 = setFromList [1, 1, 5, 1, 2, 0, 10, 4, 6]

-- >>> set1
-- Set [0,1,2,4,5,6,10]

insert :: (Ord a) => a -> Set a -> Set a
insert x (Set xs) = Set (insert' x xs)
  where
    insert' :: (Ord a) => a -> [a] -> [a]
    insert' x [] = [x]
    insert' x l@(y : ys) =
      case cmp x y of
        Equal -> l
        LessThan -> x : y : ys
        GreaterThan -> y : insert' x ys

-- >>> insert 10 set1
-- Set [0,1,2,4,5,6,10]

-- >>> insert 9 set1
-- Set [0,1,2,4,5,6,9,10]

-- >>> insert (-1) set1
-- Set [-1,0,1,2,4,5,6,10]

-- >>> insert 1 emptySet
-- Set [1]

delete :: (Eq a) => a -> Set a -> Set a
delete x (Set xs) = Set (delete' x xs)
  where
    delete' :: (Eq a) => a -> [a] -> [a]
    delete' _ [] = []
    delete' x (y : ys)
      | x == y = ys
      | otherwise = y : delete' x ys

-- >>> delete 2 set1
-- Set [0,1,4,5,6,10]

-- >>> delete 3 set1
-- Set [0,1,2,4,5,6,10]

-- >>> delete 1 (singletonSet 1)
-- Set []

elemS :: (Eq a) => a -> Set a -> Bool
elemS x (Set xs) = x `elem` xs

-- >>> 5 `elemS` set1
-- True

-- >>> 5 `elemS` emptySet
-- False

-- обединението е почти еквивалентно на алгоритъма
-- за сливане на сортирани списъци, с разликата че
-- тук не добавяме равни елементи по няколко пъти
union :: (Ord a) => Set a -> Set a -> Set a
union (Set xs) (Set ys) = Set (mergeUnique xs ys)
  where
    mergeUnique :: (Ord a) => [a] -> [a] -> [a]
    mergeUnique [] l2 = l2
    mergeUnique l1 [] = l1
    mergeUnique l1@(x : xs) l2@(y : ys)
      -- или с Ordering
      | x == y = mergeUnique xs l2
      | x < y = x : mergeUnique xs l2
      | x > y = y : mergeUnique l1 ys

set2 :: Set Int
set2 = setFromList [9, 1, 5, 3, 2, 4, 8]

-- >>> set2
-- Set [1,2,3,4,5,8,9]

-- >>> union set1 set2
-- Set [0,1,2,3,4,5,6,8,9,10]

-- >>> union set2 set2
-- Set [1,2,3,4,5,8,9]

-- >>> union emptySet set2
-- Set [1,2,3,4,5,8,9]

-- >>> union (singletonSet 6) set2
-- Set [1,2,3,4,5,6,8,9]

intersect :: (Ord a) => Set a -> Set a -> Set a
intersect (Set xs) (Set ys) = Set (intersect' xs ys)
  where
    intersect' :: (Ord a) => [a] -> [a] -> [a]
    intersect' [] _ = []
    intersect' _ [] = []
    intersect' l1@(x : xs) l2@(y : ys) =
      case cmp x y of
        Equal -> x : intersect' xs ys
        LessThan -> intersect' xs l2
        GreaterThan -> intersect' l1 ys

-- >>> intersect set1 set2
-- Set [1,2,4,5]

-- >>> intersect emptySet set1
-- Set []

-- >>> intersect set1 (singletonSet 4)
-- Set [4]

-- тук приемаме, че множествата са създадени
-- чрез fromList, т.е. че са сортирани и без
-- повторения
equal :: (Eq a) => Set a -> Set a -> Bool
equal (Set xs) (Set ys) = xs == ys

-- >>> equal set1 set2
-- False

-- >>> equal set2 set2
-- True

-- >>> equal set2 (1 `insert` set2)
-- True

-- >>> equal emptySet emptySet
-- True

-- >>> equal emptySet (singletonSet 1)
-- False

-- би било ограничение да искаме Dict да е инстанция
-- на Eq или Ord (или поне тези, които Haskell генерира автоматично за нас),
-- тъй като списъците и оттам наредените двойки биха се сравнили
-- покомпонентно, а ние искаме само ключовете да са сравними
newtype Dict k v = Dict [(k, v)]
  deriving (Show)

emptyDict :: Dict k v
emptyDict = Dict []

singletonDict :: k -> v -> Dict k v
singletonDict key value = Dict [(key, value)]

-- това тук е изключително грозно и трудоемко,
-- по принцип можем да обединим тази имплементация
-- и предишната, подавайки предикат (функция от по-висок ред)
quicksortKvp :: (Ord k) => [(k, v)] -> [(k, v)]
quicksortKvp [] = []
quicksortKvp (x : xs) = quicksortKvp less ++ [x] ++ quicksortKvp greater
  where
    less = filterLessEq x xs
    greater = filterGreater x xs

    filterLessEq :: (Ord k) => (k, v) -> [(k, v)] -> [(k, v)]
    filterLessEq _ [] = []
    filterLessEq x@(key, _) (kvp@(key', _) : kvps)
      | key' <= key = kvp : filterLessEq x kvps
      | otherwise = filterLessEq x kvps

    filterGreater :: (Ord k) => (k, v) -> [(k, v)] -> [(k, v)]
    filterGreater _ [] = []
    filterGreater x@(key, _) (kvp@(key', _) : kvps)
      | key' > key = kvp : filterGreater x kvps
      | otherwise = filterGreater x kvps

-- >>> quicksortKvp [(1,'a'),(10,'b'),(4,'i'),(2,'h')]
-- [(1,'a'),(2,'h'),(4,'i'),(10,'b')]

cmpKvp :: (Ord k) => (k, v) -> (k, v) -> Ordering
cmpKvp (key1, _) (key2, _) = cmp key1 key2

-- >>> cmpKvp (1,4) (-1,3)
-- GreaterThan

elemKvp :: (Eq k) => (k, v) -> [(k, v)] -> Bool
elemKvp _ [] = False
elemKvp kvp@(key, _) ((key', _) : kvps) =
  key == key' || elemKvp kvp kvps

-- >>> (1,'h') `elemKvp` [(1,'t'),(2,'t')]
-- True

dictFromList :: (Ord k) => [(k, v)] -> Dict k v
dictFromList kvps = Dict (quicksortKvp (filterKvps kvps))
  where
    filterKvps :: (Eq k) => [(k, v)] -> [(k, v)]
    filterKvps [] = []
    filterKvps (kvp : kvps)
      | kvp `elemKvp` kvps = filterKvps kvps
      | otherwise = kvp : filterKvps kvps

dict1 :: Dict Int String
dict1 =
  dictFromList
    [ (3, "aa"),
      (6, "xyz"),
      (1, "st"),
      (7, "rrs"),
      (0, "fg"),
      (0, "ff")
    ]

-- >>> dict1
-- Dict [(0,"ff"),(1,"st"),(3,"aa"),(6,"xyz"),(7,"rrs")]

dictToList :: Dict k v -> [(k, v)]
dictToList (Dict kvps) = kvps

-- >>> dictToList dict1
-- [(0,"ff"),(1,"st"),(3,"aa"),(6,"xyz"),(7,"rrs")]

insertDict :: (Ord k) => k -> v -> Dict k v -> Dict k v
insertDict key value (Dict dict) = Dict (insert' key value dict)
  where
    insert' :: (Ord k) => k -> v -> [(k, v)] -> [(k, v)]
    insert' key value [] = [(key, value)]
    insert' key value (kvp@(key', _) : kvps) =
      case cmp key key' of
        Equal -> (key, value) : kvps
        LessThan -> (key, value) : kvp : kvps
        GreaterThan -> kvp : insert' key value kvps

-- >>> insertDict 1 "qw" dict1
-- Dict [(0,"ff"),(1,"qw"),(3,"aa"),(6,"xyz"),(7,"rrs")]

-- >>> insertDict 2 4 emptyDict
-- Dict [(2,4)]

-- >>> insertDict 2 "rst" (insertDict 3 "abc" (singletonDict 1 "xyz"))
-- Dict [(1,"xyz"),(2,"rst"),(3,"abc")]

deleteDict :: (Eq k) => k -> Dict k v -> Dict k v
deleteDict key (Dict kvps) = Dict (delete' key kvps)
  where
    delete' :: (Eq k) => k -> [(k, v)] -> [(k, v)]
    delete' _ [] = []
    delete' key (kvp@(key', _) : kvps) =
      if key == key'
        then kvps
        else kvp : delete' key kvps

-- >>> deleteDict 6 dict1
-- Dict [(0,"ff"),(1,"st"),(3,"aa"),(7,"rrs")]

-- >>> deleteDict 3 (deleteDict 7 dict1)
-- Dict [(0,"ff"),(1,"st"),(6,"xyz")]

-- >>> deleteDict 4 emptyDict
-- Dict []

-- >>> deleteDict 4 (singletonDict 4 'k')
-- Dict []

lookupDict :: (Eq k) => k -> Dict k v -> Maybe v
lookupDict key (Dict kvps) = lookup' key kvps
  where
    lookup' :: (Eq k) => k -> [(k, v)] -> Maybe v
    lookup' _ [] = Nothing
    lookup' key ((key', value') : kvps) =
      if key == key'
        then Just value'
        else lookup' key kvps

-- >>> lookupDict 1 dict1
-- Just "st"

-- >>> lookupDict 10 dict1
-- Nothing

-- >>> lookupDict 10 emptyDict
-- Nothing

-- >>> lookupDict 2 (singletonDict 2 '8')
-- Just '8'

merge :: (Ord k) => Dict k v -> Dict k v -> Dict k v
merge (Dict kvps1) (Dict kvps2) = Dict (merge' kvps1 kvps2)
  where
    merge' :: (Ord k) => [(k, v)] -> [(k, v)] -> [(k, v)]
    merge' [] l2 = l2
    merge' l1 [] = l1
    merge' l1@(kvp1@(key1, _) : kvps1) l2@(kvp2@(key2, _) : kvps2) =
      case cmp key1 key2 of
        Equal -> kvp2 : merge' kvps1 kvps2
        LessThan -> kvp1 : merge' kvps1 l2
        GreaterThan -> kvp2 : merge' l1 kvps2

dict2 :: Dict Int String
dict2 =
  dictFromList
    [ (4, "aba"),
      (2, "xqz"),
      (1, "sj"),
      (9, "rrs"),
      (0, "fw"),
      (3, "rr")
    ]

-- >>> merge dict1 dict2
-- Dict [(0,"fw"),(1,"sj"),(2,"xqz"),(3,"rr"),(4,"aba"),(6,"xyz"),(7,"rrs"),(9,"rrs")]

-- >>> merge emptyDict emptyDict
-- Dict []

-- >>> merge emptyDict (singletonDict 7 'p')
-- Dict [(7,'p')]

-- >>> merge (singletonDict 7 'p') (singletonDict 7 'r')
-- Dict [(7,'r')]
