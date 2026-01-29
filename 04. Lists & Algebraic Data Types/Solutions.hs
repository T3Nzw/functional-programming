module Solutions where

import Data.Maybe (isJust)
import Prelude hiding (zip)

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

-- >>> zip [1,2,3] "abcd"
-- [(1,'a'),(2,'b'),(3,'c')]

longest :: [[a]] -> [a]
longest = longest' 0 []
 where
  longest' :: Int -> [a] -> [[a]] -> [a]
  longest' _ acc [] = acc
  longest' len acc (x : xs) =
    let len2 = length x
     in if len2 > len
          then longest' len2 x xs
          else longest' len acc xs

-- >>> longest ["abc", "abcd", "ab", "cab", "abbc"]
-- "abcd"

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort lst = minEl : selectionSort (removeFirstOccurenceOfMinElement lst)
 where
  minEl = minimum lst

  removeFirstOccurenceOfMinElement [] = []
  removeFirstOccurenceOfMinElement (x : xs)
    | x == minEl = xs
    | otherwise = x : removeFirstOccurenceOfMinElement xs

-- >>> selectionSort [3,1,5,2,6]
-- [1,2,3,5,6]

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct [] _ = []
cartesianProduct (x : xs) l2 = helper x l2 ++ cartesianProduct xs l2
 where
  helper :: a -> [b] -> [(a, b)]
  helper _ [] = []
  helper x (y : ys) = (x, y) : helper x ys

-- >>> cartesianProduct [1,2,3] [4,5]
-- [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]

-- чрез отделяне на списъци (list comprehension)
-- fun fact: терминът "отделяне на списъци" (list comprehension)
-- идва от теория на множествата и по-специално
-- от аксиомата за отделянето
-- (axiom of (restricted) comprehension),
-- затова този запис толкова прилича на т.нар.
-- set-builder notation
cartesianProduct2 :: [a] -> [b] -> [(a, b)]
cartesianProduct2 l1 l2 = [(x, y) | x <- l1, y <- l2]

-- >>> cartesianProduct2 [1,2,3] [4,5]
-- [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]

-- чрез функции от по-висок ред (това за другия път)
cartesianProduct3 :: [a] -> [b] -> [(a, b)]
cartesianProduct3 l1 l2 = concatMap (\x -> map (x,) l2) l1

-- >>> cartesianProduct3 [1,2,3] [4,5]
-- [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]

type Frequency = Int

histogram :: Eq a => [a] -> [(Frequency, a)]
histogram lst = histogram' [] lst
 where
  histogram' :: Eq a => [(Frequency, a)] -> [a] -> [(Frequency, a)]
  histogram' acc [] = acc
  histogram' acc (x : xs) =
    histogram' (incAtKey x acc) xs

  incAtKey :: Eq a => a -> [(Frequency, a)] -> [(Frequency, a)]
  incAtKey x [] = [(1, x)]
  incAtKey x (kvp@(n, value) : xs) =
    if x == value
      then (n + 1, value) : xs
      else kvp : incAtKey x xs

-- >>> histogram "abcddaba"
-- [(3,'a'),(2,'b'),(1,'c'),(2,'d')]

binary :: [String]
binary = "1" : binary' binary
 where
  binary' :: [String] -> [String]
  binary' [] = []
  binary' (x : xs) = (x ++ "0") : (x ++ "1") : binary' xs

-- >>> take 20 binary
-- ["1","10","11","100","101","110","111","1000","1001","1010","1011","1100","1101","1110","1111","10000","10001","10010","10011","10100"]

-- >>> binary !! 1023
-- "10000000000"

newtype Matrix = Matrix [[Double]]
  deriving (Show, Eq)

isEmpty :: Matrix -> Bool
isEmpty (Matrix m) = null m

isMatrix :: Matrix -> Bool
isMatrix (Matrix m) = checkDim (length m) m
 where
  checkDim :: Int -> [[Double]] -> Bool
  checkDim _ [] = True
  checkDim len (x : xs) = len == length x && checkDim len xs

isMatrix2 :: Matrix -> Bool
isMatrix2 (Matrix m) = let len = length m in all (== len) $ map length m

isMatrix3 :: Matrix -> Bool
isMatrix3 (Matrix m) = let len = length m in all ((== len) . length) m

addMatrices :: Matrix -> Matrix -> Matrix
addMatrices (Matrix m1) (Matrix m2) = Matrix $ addMatrices' m1 m2
 where
  addMatrices' :: [[Double]] -> [[Double]] -> [[Double]]
  addMatrices' (x : xs) (y : ys) = addRows x y : addMatrices' xs ys
  addMatrices' _ _ = []

  addRows :: [Double] -> [Double] -> [Double]
  addRows (x : xs) (y : ys) = x + y : addRows xs ys
  addRows _ _ = []

type Rows = Int

type Cols = Int

dimensions :: Matrix -> Maybe (Rows, Cols)
dimensions m@(Matrix xs)
  | isEmpty m = Just (0, 0)
  | isMatrix m = Just (length xs, length (head xs))
  | otherwise = Nothing

-- горната функция ще върне грешен резултат,
-- ако се опитаме да съберем матрици с различни
-- размерности. в този случай бихме искали да съобщим
-- за грешка по някакъв начин и този начин във ФП
-- е чрез АТД Maybe
addMatrices2 :: Matrix -> Matrix -> Maybe Matrix
addMatrices2 m1 m2
  | let d1 = dimensions m1
     in isJust d1 && d1 == dimensions m2 =
      Just $ addMatrices m1 m2
  | otherwise = Nothing

matrix1 :: Matrix
matrix1 =
  Matrix
    [ [1, 2, 3]
    , [4, 5, 6]
    , [7, 8, 9]
    ]

matrix2 :: Matrix
matrix2 =
  Matrix
    [ [1, 2]
    , [3, 4]
    ]

-- >>> isMatrix matrix1 && isMatrix matrix2
-- True

-- >>> let Matrix m = matrix1 in isMatrix $ Matrix $ tail (head m) : tail m
-- False

-- >>> addMatrices matrix1 matrix2
-- Matrix [[2.0,4.0],[7.0,9.0]]

-- очевидно не трябва да можем да добавим две матрици с размерности
-- респективно 3х3 и 2х2

-- >>> addMatrices2 matrix1 matrix2
-- Nothing

-- >>> addMatrices2 matrix1 matrix1
-- Just (Matrix [[2.0,4.0,6.0],[8.0,10.0,12.0],[14.0,16.0,18.0]])

transpose :: Matrix -> Matrix
transpose (Matrix m) = Matrix $ go m
 where
  go :: [[Double]] -> [[Double]]
  go [] = []
  go ([] : _) = []
  go m = map head m : go (map tail m)

-- >>> transpose matrix1
-- Matrix [[1.0,4.0,7.0],[2.0,5.0,8.0],[3.0,6.0,9.0]]

scalarProduct :: Num a => [a] -> [a] -> a
scalarProduct = (sum .) . zipWith (*)

multMatrices :: Matrix -> Matrix -> Maybe Matrix
multMatrices m1' m2'
  | not (validDimensions m1' m2') = Nothing
  | otherwise =
      let Matrix m1 = m1'
          Matrix m2 = transpose m2'
       in Just . Matrix $ mul m1 m2
 where
  mul :: [[Double]] -> [[Double]] -> [[Double]]
  mul m1 m2 = map (\row -> map (scalarProduct row) m2) m1

  validDimensions :: Matrix -> Matrix -> Bool
  validDimensions m1 m2 =
    let d1 = dimensions m1
        d2 = dimensions m2
     in case (d1, d2) of
          (Just (_, cols1), Just (rows2, _)) -> cols1 == rows2
          _ -> False

-- >>> multMatrices matrix1 matrix1
-- Just (Matrix [[30.0,36.0,42.0],[66.0,81.0,96.0],[102.0,126.0,150.0]])
