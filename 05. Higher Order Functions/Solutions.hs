module Solutions where

import Prelude hiding (all, any, concatMap, curry, filter, foldl, foldr, iterate, map, scanr, uncurry, zipWith, (.))

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x : xs)
  | p x = x : filter p xs
  | otherwise = filter p xs

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : map f xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op nv [] = nv
foldr op nv (x : xs) = x `op` foldr op nv xs

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl op nv [] = nv
foldl op nv (x : xs) = foldl op (op nv x) xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p lst = foldr (\x xs -> if p x then x : xs else xs) [] lst

map' :: (a -> b) -> [a] -> [b]
map' f lst = foldr (\x xs -> f x : xs) [] lst

-- compose
(.) :: (b -> c) -> (a -> b) -> (a -> c)
g . f = \x -> g (f x)

all :: (a -> Bool) -> [a] -> Bool
all p lst = foldr (\x xs -> p x && xs) True lst

-- >>> all' even [2,3,4,6]
-- False

any :: (a -> Bool) -> [a] -> Bool
any p lst = foldr (\x xs -> p x || xs) False lst

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f lst = foldr (\x xs -> f x ++ xs) [] lst

-- >>> concatMap (\x -> [x,-x]) [1,2,3]
-- [1,-1,2,-2,3,-3]

--- >>> concatMap (\x -> replicate x x) [0,1,2,3,4]
-- [1,2,2,3,3,3,4,4,4,4]

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith op l1 l2 = foldr ((:) . uncurry op) [] $ l1 `zip` l2

-- >>> zipWith' (+) [1..10] [-1,-2..(-10)]
-- [0,0,0,0,0,0,0,0,0,0]

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

-- >>> take 10 $ iterate (+1) 1
-- [1,2,3,4,5,6,7,8,9,10]

nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy p l = foldr (\x xs -> if any (p x) xs then xs else x : xs) [] l

-- >>> nubBy (==) [1,1,1,3,1,10]
-- [3,1,10]

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f x =
  case f x of
    Nothing -> []
    Just (h, t) -> h : unfoldr f t

(&) :: a -> (a -> b) -> b
x & f = f x

-- задаване на лявоасоциативност (l в infixl)
-- и приоритет (1) на оператора
infixl 1 &

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
f &&& g = \x -> (f x, g x)

fixpoints :: (Eq a) => (a -> a) -> [a] -> [a]
fixpoints f lst = foldr (\x xs -> if f x == x then x : xs else xs) [] lst

-- >>> fixpoints (`mod` 2) [0,1,2,3,4]
-- [0,1]

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr op nv lst = foldr (\x xs@(y : _) -> x `op` y : xs) [nv] lst

-- >>> scanr (+) 0 [1,2,3]
-- [6,5,3,0]

-- >>> scanr (:) [] [1,2,3]
-- [[1,2,3],[2,3],[3],[]]

composeL :: [a -> a] -> (a -> a)
composeL fs = foldr (.) id fs

-- >>> composeL [(+1), (*3), (^2), (subtract 3)] 9
-- 109

selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort lst = l1 ++ selectionSort l2
  where
    minEl = minimum lst
    l1 = filter (== minEl) lst
    l2 = filter (/= minEl) lst

-- >>> selectionSort [4,2,3,1]
-- [1,2,3,4]

insert :: (Ord a) => a -> [a] -> [a]
insert x l = filter (<= x) l ++ [x] ++ filter (> x) l

insertOrdered :: (Ord a) => a -> [a] -> [a]
insertOrdered x [] = [x]
insertOrdered x (y : ys)
  | y <= x = y : insertOrdered x ys
  | otherwise = x : y : ys

insertionSort :: (Ord a) => [a] -> [a]
insertionSort = helper []
  where
    helper :: (Ord a) => [a] -> [a] -> [a]
    helper sorted [] = sorted
    helper sorted (x : xs) =
      helper (insertOrdered x sorted) xs

-- >>> insertionSort [4,2,3,1]
-- [1,2,3,4]

quicksortBy :: (a -> a -> Bool) -> [a] -> [a]
quicksortBy _ [] = []
quicksortBy cmp (x : xs) =
  let leq = filter (`cmp` x) xs
      ge = filter (not . (`cmp` x)) xs -- не е същото като (x `cmp`)!!!
   in quicksortBy cmp leq ++ [x] ++ quicksortBy cmp ge

find :: (a -> Bool) -> [a] -> Maybe (Int, a)
find p lst =
  foldr
    (\el@(_, x) xs -> if p x then Just el else xs)
    Nothing
    $ [0 ..] `zip` lst

-- >>> find (even) [1,2,3]
-- Just (1,2)

-- >>> find ((>= 6) . length) ["abcd", "xyz", "aaaaaa", "bbbbbbbbbbb"]
-- Just (2,"aaaaaa")

suffixes :: [a] -> [[a]]
suffixes lst = scanr (:) [] lst

-- >>> tails [1,2,3]
-- [[1,2,3],[2,3],[3],[]]

intervalsN :: Int -> [Int] -> [[Int]]
intervalsN n int =
  map (take n) . filter ((>= n) . length) $ suffixes int

-- >>> intervalsN 3 [1..8]
-- [[1,2,3],[2,3,4],[3,4,5],[4,5,6],[5,6,7],[6,7,8]]

integers :: [Int]
integers = 0 : concatMap (\x -> [x, -x]) [1 ..]

-- >>> take 20 integers
-- [0,1,-1,2,-2,3,-3,4,-4,5,-5,6,-6,7,-7,8,-8,9,-9,10]
