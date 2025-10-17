module Solutions where

import Prelude hiding (exp, pred, succ)

-- за голяма част от задачите може да се направи
-- допълнителна валидация за отрицателност на аргументите,
-- но да предположим, че в случая няма нужда от това :)

trianglePerimeter :: Double -> Double -> Double -> Double
trianglePerimeter a b c = a + b + c

-- >>> trianglePerimeter 2 2 3
-- 7.0

triangleArea :: Double -> Double -> Double -> Double
triangleArea a b c = sqrt (semiperimeter * diff1 * diff2 * diff3)
  where
    semiperimeter = trianglePerimeter a b c / 2 -- функцията е с най-голям приоритет!
    diff1 = semiperimeter - a
    diff2 = semiperimeter - b
    diff3 = semiperimeter - c

-- >>> triangleArea 2 2 3
-- 1.984313483298443

type Point = (Double, Double)

sideLength :: Point -> Point -> Double
sideLength (x1, y1) (x2, y2) = sqrt (xd ^ 2 + yd ^ 2)
  where
    xd = x1 - x2
    yd = y1 - y2

trianglePerimeter' :: Point -> Point -> Point -> Double
trianglePerimeter' p1 p2 p3 = trianglePerimeter side1 side2 side3
  where
    side1 = sideLength p1 p2
    side2 = sideLength p1 p3
    side3 = sideLength p2 p3

-- >>> trianglePerimeter' (0,1) (2,8) (5,6)
-- 17.95672897660998

triangleArea' :: Point -> Point -> Point -> Double
triangleArea' p1 p2 p3 = triangleArea side1 side2 side3
  where
    side1 = sideLength p1 p2
    side2 = sideLength p1 p3
    side3 = sideLength p2 p3

-- >>> triangleArea' (0,1) (2,8) (5,6)
-- 12.49999999999999

printStudent :: (String, String, String, Int) -> String
printStudent (name, fn, major, year) =
  "This is "
    ++ name
    ++ " with a faculty number of "
    ++ fn
    ++ " who is in year "
    ++ show year
    ++ " of "
    ++ major

-- >>> printStudent ("imbadwithnames", "99999", "Computer Science", 3)
-- "This is imbadwithnames with a faculty number of 99999 who is in year 3 of Computer Science"

succ :: Int -> Int
succ n = n + 1

pred :: Int -> Int
pred n = helper 0 n
  where
    helper :: Int -> Int -> Int
    helper acc n
      | n <= 0 = 0
      | succ acc == n = acc
      | otherwise = helper (succ acc) n

-- >>> pred 6
-- 5

add :: Int -> Int -> Int
add 0 m = m
add n m = succ (add (pred n) m)

-- >>> add 99 21
-- 120

mult :: Int -> Int -> Int
mult 0 m = 0
mult n m = add m (mult (pred n) m)

-- >>> mult 10 12
-- 120

exp :: Int -> Int -> Int
exp n 0 = 1
exp n m = mult n (exp n (pred m))

-- >>> exp 2 12
-- 4096

digitSum :: Int -> Int
digitSum 0 = 0
digitSum n = abs n `mod` 10 + digitSum (n `quot` 10)

-- >>> digitSum 12345
-- 15

intervalSum :: Int -> Int -> Int
intervalSum a b
  | a > b = 0
  | otherwise = a + intervalSum (a + 1) b

-- >>> intervalSum (-1) 16
-- 135

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

-- >>> fact 6
-- 720

doubleFact :: Int -> Int
doubleFact 0 = 1
doubleFact 1 = 1
doubleFact n = n * doubleFact (n - 2)

-- >>> doubleFact 6
-- 48

gcd' :: Int -> Int -> Int
gcd' 0 0 = error "numbers cannot be both zero"
gcd' a 0 = abs a
gcd' 0 b = abs b
gcd' a b
  | a > b = gcd' (a - b) b
  | otherwise = gcd' a (b - a)

gcd'' :: Int -> Int -> Int
gcd'' 0 0 = error "both numbers cannot be zero"
gcd'' a 0 = abs a
gcd'' 0 b = abs b
gcd'' a b
  | a > b = gcd'' b (a `mod` b)
  | otherwise = gcd'' a (b `mod` a)

-- >>> gcd' 16 8
-- 8

-- >>> gcd'' 15 9
-- 3

lcm' :: Int -> Int -> Int
lcm' a b = abs (a * b `div` gcd' a b)

-- >>> lcm' 12 16
-- 48

fastPow :: Double -> Int -> Double
fastPow x 0 = 1
fastPow x n
  | n < 0 = fastPow (1 / x) (abs n)
  | even n = fastPow (x ^ 2) (n `div` 2)
  | otherwise = x * fastPow x (n - 1)

-- >>> fastPow 4 2
-- 16.0
-- >>> fastPow 3 (-1)
-- 0.3333333333333333

coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1

totient :: Int -> Int
totient 0 = 1
totient n = helper 1 n
  where
    helper :: Int -> Int -> Int
    helper acc n
      | acc >= n = 0
      | otherwise =
          if coprime acc n
            then 1 + helper (succ acc) n
            else helper (succ acc) n

-- >>> totient 7
-- 6

prime :: Int -> Bool
prime n = helper 2 n
  where
    helper :: Int -> Int -> Bool
    helper acc n
      | n <= 1 = False
      | acc >= n = True
      | otherwise = coprime acc n && helper (acc + 1) n

-- >>> prime 12
-- False

goldbach :: Int -> (Int, Int)
goldbach = helper 2
  where
    helper :: Int -> Int -> (Int, Int)
    helper acc n
      | acc >= n = error "oops"
      | prime acc && prime (n - acc) = (acc, n - acc)
      | otherwise = helper (succ acc) n

-- >>> goldbach 14
-- (3,11)
