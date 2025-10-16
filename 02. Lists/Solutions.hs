module Solutions where

-- решенията са кратки, обещавам, повечето неща са коментари с допълнителни обяснения :)

import Prelude hiding (drop, sum, take)

{-
Обещаните разлики между rem и mod
и quot и div от семинара:

> mod/rem
  > mod ни връща остатък със знак, който съвпада с този на десния аргумент
  > rem ни връща остатък със знак, който съвпада с този на левия аргумент

> quot/div
  > quot закръгля към 0
  > div закръгля към минус безкрайност

Ако сте в настроение за по-математическата дефиниция:
https://stackoverflow.com/questions/5891140/difference-between-mod-and-rem-in-haskell
-}

-- >>> 1 `mod` 2
-- 1
-- >>> 1 `rem` 2
-- 1

-- НО,

-- >>> 1 `mod` (-2)
-- -1
-- >>> 1 `rem` (-2)
-- 1

-- >>> (-1) `mod` 2
-- 1
-- >>> (-1) `rem` 2
-- -1

-- >>> (-1) `mod` (-2)
-- -1
-- >>> (-1) `rem` (-2)
-- -1

-- >>> 1 `div` 10
-- 0
-- >>> 1 `quot` 10
-- 0

-- НО,

-- >>> (-1) `div` 10
-- -1
-- >>> (-1) `quot` 10
-- 0

reverseNumber :: Int -> Int
reverseNumber 0 = 0
reverseNumber n = helper 0 n
  where
    helper :: Int -> Int -> Int
    helper acc 0 = acc
    helper acc n = helper (acc * 10 + n `rem` 10) (n `quot` 10)

palindrome :: Int -> Bool
palindrome n = n == reverseNumber n

-- нулата при n е проблем, защото всяко ненулево число
-- може да се представи по безкраен брой начини
-- като суми от естествени числа, тъй като
-- x^0 == 1 за всяко x>0.
-- ако x==0, за всяка ненулева степен е вярно,
-- че 0 == 0^n.
-- забележете, че следните две функции БРОЯТ
-- 0 като част от сумите. по-надолу са модифицираните
-- версии, които не я включват в сумите (т.е. съвпадат с изхода
-- за примерните входове).
-- това, че включваме нулата, означава, че броят суми,
-- които получаваме като резултат, се удвояват, тъй като
-- нулата не променя сумите и оттам участва и не участва
-- във всяка сума точно по веднъж
sumOfNthPowers :: Int -> Int -> Int
sumOfNthPowers x n = helper 0 x n
  where
    helper :: Int -> Int -> Int -> Int
    helper acc x n
      | n <= 0 = error "oops"
      | x < 0 = 0
      | x == 0 = 1
      | acc > x = 0
      | otherwise =
          helper (succ acc) (x - acc ^ n) n
            + helper (succ acc) x n

-- тук БРОИМ и нулата :)
-- >>> sumOfNthPowers 10 2
-- 2
-- >>> sumOfNthPowers 8 2
-- 0
-- >>> sumOfNthPowers 0 2
-- 1
-- >>> sumOfNthPowers 0 10
-- 1
-- >>> sumOfNthPowers 8 3
-- 2
-- >>> sumOfNthPowers (-1) 10
-- 0
-- >>> sumOfNthPowers 25 2
-- 4
-- >>> sumOfNthPowers 30 2
-- 4

-- тази задача е аналогична на горната, с разликата
-- че тук връщаме свидетел за горния резултат.
-- забележете, че е изпълнено
-- sumOfNthPowers x n == length (sumOfNthPowersL x n)
sumOfNthPowersL :: Int -> Int -> [[Int]]
sumOfNthPowersL x n
  -- това е малко досадно, но иначе не бихме добавили 0 в списъка,
  -- ако x == 0, т.е. трябва да разгледаме случая за x == 0 още
  -- преди да извикаме helper. също така, причината да не
  -- pattern match-ваме, е заради грешката за n <= 0 (за да сме консистентни)
  | n <= 0 = error "oops"
  | x == 0 = [[x]]
  | otherwise = helper [] 0 x n
  where
    helper :: [Int] -> Int -> Int -> Int -> [[Int]]
    helper l acc x n
      | n <= 0 = error "oops"
      | x < 0 = []
      | x == 0 = [l]
      | acc > x = []
      | otherwise =
          helper (acc : l) (succ acc) (x - acc ^ n) n
            ++ helper l (succ acc) x n

-- свидетели за горните резултати
-- >>> sumOfNthPowersL 10 2
-- [[3,1,0],[3,1]]
-- >>> sumOfNthPowersL 8 2
-- []
-- >>> sumOfNthPowersL 8 3
-- [[2,0],[2]]
-- >>> sumOfNthPowersL 0 2
-- [[0]]
-- >>> sumOfNthPowersL 0 10
-- [[0]]
-- >>> sumOfNthPowersL (-1) 10
-- []
-- >>> sumOfNthPowersL 25 2
-- [[4,3,0],[5,0],[4,3],[5]]
-- >>> sumOfNthPowersL 30 2
-- [[4,3,2,1,0],[5,2,1,0],[4,3,2,1],[5,2,1]]

-- тук не броим нулата в сумите.
-- аналогично, не искаме x да е 0
sumOfNthPowers' :: Int -> Int -> Int
-- можем и да върнем грешка за x == 0 и n <= 0,
-- аналогично на горната имплементация.
-- implementation is left as an exercise to the reader :)
sumOfNthPowers' 0 _ = 0
sumOfNthPowers' x n = helper 1 x n
  where
    helper :: Int -> Int -> Int -> Int
    helper acc x n
      | n <= 0 = error "oops"
      | x < 0 = 0
      | x == 0 = 1
      | acc > x = 0
      | otherwise =
          helper (succ acc) (x - acc ^ n) n
            + helper (succ acc) x n

-- >>> sumOfNthPowers' 10 2
-- 1
-- >>> sumOfNthPowers' 8 2
-- 0
-- >>> sumOfNthPowers' 8 3
-- 1
-- >>> sumOfNthPowers' 0 2
-- 0
-- >>> sumOfNthPowers' 0 10
-- 0
-- >>> sumOfNthPowers' (-1) 10
-- 0
-- >>> sumOfNthPowers' 25 2
-- 2
-- >>> sumOfNthPowers' 30 2
-- 2

sumOfNthPowersL' :: Int -> Int -> [[Int]]
sumOfNthPowersL' 0 _ = []
sumOfNthPowersL' x n = helper [] 1 x n
  where
    helper :: [Int] -> Int -> Int -> Int -> [[Int]]
    helper l acc x n
      | n <= 0 = error "oops"
      | x < 0 = []
      | x == 0 = [l]
      | acc > x = []
      | otherwise =
          helper (acc : l) (succ acc) (x - acc ^ n) n
            ++ helper l (succ acc) x n

-- >>> sumOfNthPowersL' 10 2
-- [[3,1]]
-- >>> sumOfNthPowersL' 8 2
-- []
-- >>> sumOfNthPowersL' 8 3
-- [[2]]
-- >>> sumOfNthPowersL' 0 2
-- []
-- >>> sumOfNthPowersL' 0 10
-- []
-- >>> sumOfNthPowersL' (-1) 10
-- []
-- >>> sumOfNthPowersL' 25 2
-- [[4,3],[5]]
-- >>> sumOfNthPowersL' 30 2
-- [[4,3,2,1],[5,2,1]]

-- за упражнение може да си напишете една помощна функция за по-лесни проверки
-- за горните функции, т.е. дали sumOfNthPowers x n == length (sumOfNthPowersL x n),
-- или пък функция, която приема списък от всички такива комбинации (sumOfNthPowersL)
-- и проверява дали е вярно, че за всяка такава комбинация след повдигане на всеки елемент
-- на степен n и сумирането на резултатите получаваме числото x.
-- разбира се, не е толкова тривиално да се провери дали горните функции генерират
-- ВСИЧКИ възможни такива комбинации

sum :: [Int] -> Int
sum [] = 0
sum (x : xs) = x + sum xs

-- >>> sum [1..10]
-- 55

{-
Имаше бонус условие, в което се искаше следните функции да
могат да работят за списъци от произволни типове.
Това е т.нар. полиморфизъм в Haskell (разбира се, не само тук :D).
'a' в типовете на следните функции се нарича типова променлива
и при извикване на функцията се инстанцира с някакъв конкретен тип,
напр. Int, Char и т.н. Ще го говорим на някое от следващите упражнения
-}

append :: [a] -> [a] -> [a]
append [] l2 = l2
append (x : xs) l2 = x : append xs l2

-- >>> append [1,2,3] [4,5]
-- [1,2,3,4,5]
-- >>> append [1,2] []
-- [1,2]

take :: Int -> [a] -> [a]
take _ [] = []
take n l@(x : xs)
  | n <= 0 = []
  | otherwise = x : take (pred n) xs

-- >>> take 2 [1,2,3]
-- [1,2]
-- >>> take 10 [1,2,3]
-- [1,2,3]
-- >>> take 1 []
-- []
-- >>> take (-1) []
-- []

drop :: Int -> [a] -> [a]
drop _ [] = []
drop n l@(_ : xs)
  | n <= 0 = l
  | otherwise = drop (pred n) xs

-- >>> drop 1 [1,2,3]
-- [2,3]
-- >>> drop 10 [1..11]
-- [11]
-- >>> drop 4 []
-- []
-- >>> drop (-1) [1,2,3]
-- [1,2,3]

-- забележете, че за да можем да разделим някакъв списък
-- на по-малки списъци на база проверка за равенство между
-- разделителя и някой елемент на списъка, трябва да можем
-- да сравняваме елементите на този списък, т.е. типът,
-- с който заместваме типовата променлива а, трябва да
-- има стойности, които могат да бъдат сравнявани.
-- затова използваме т.нар. ограничения (преди удебелената стрелка =>).
-- в случая искаме конкретния тип да има дефиниран оператор за сравнение
-- и такова ограничение може да бъде наложено чрез типовия клас Eq (:i Eq в GHCi).
-- това е т.нар. ad hoc полиморфизъм (за разлика от по-горните примери, където
-- полиморфизмът е параметричен)
-- в C++ аналогичен е operator overloading-ът (т.е. за да можем да сравняваме стойности
-- от даден тип, трябва да сме дефинирали bool operator==(T const &value) const {...})
split :: (Eq a) => a -> [a] -> [[a]]
split delim lst = helper [] delim lst
  where
    helper :: (Eq a) => [a] -> a -> [a] -> [[a]]
    helper acc _ [] = [acc]
    helper acc delim (x : xs)
      | x == delim = if null acc then helper [] delim xs else acc : helper [] delim xs
      | otherwise = helper (acc ++ [x]) delim xs

-- >>> split ' ' "i love  programming in    haskell <3"
-- ["i","love","programming","in","haskell","<3"]

splitEvenOdd :: [a] -> ([a], [a])
splitEvenOdd lst = helper [] [] lst
  where
    helper evenAcc oddAcc [] = (evenAcc, oddAcc)
    helper evenAcc oddAcc [x] = (evenAcc ++ [x], oddAcc)
    helper evenAcc oddAcc (x : y : xs) =
      helper (evenAcc ++ [x]) (oddAcc ++ [y]) xs

-- >>> splitEvenOdd [1..6]
-- ([1,3,5],[2,4,6])

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes l@(x : xs) =
  l : suffixes xs

-- >>> suffixes [1,2,3]
-- [[1,2,3],[2,3],[3],[]]

prefixes :: [a] -> [[a]]
prefixes l = helper [] l
  where
    helper :: [a] -> [a] -> [[a]]
    helper acc [] = [acc]
    helper acc (x : xs) = acc : helper (acc ++ [x]) xs

-- >>> prefixes [1,2,3]
-- [[],[1],[1,2],[1,2,3]]

removeConsecutive :: (Eq a) => [a] -> [a]
removeConsecutive [] = []
removeConsecutive [x] = [x]
removeConsecutive (x : xs@(y : _)) =
  if x == y
    then removeConsecutive xs
    else x : removeConsecutive xs

-- >>> removeConsecutive [1,1,2,2,2,2,4,1,1]
-- [1,2,4,1]

pack :: (Eq a) => [a] -> [[a]]
pack l = helper [] l
  where
    helper :: (Eq a) => [a] -> [a] -> [[a]]
    helper acc [] = [acc]
    helper acc [x] = [acc ++ [x]]
    helper acc (x : xs@(y : _))
      | x == y = helper (acc ++ [x]) xs
      | otherwise = (acc ++ [x]) : helper [] xs

-- >>> pack "aabbbcdda"
-- ["aa","bbb","c","dd","a"]

recaman :: Int -> Int
recaman n = helper [0] 0 n
  where
    helper :: [Int] -> Int -> Int -> Int
    helper sequence index n
      | n < 0 = error "invalid element index"
      | index > n = lastElement
      | diffElement > 0 && diffElement `notElem` sequence = helper (diffElement : sequence) (index + 1) n
      | otherwise = helper (lastElement + index : sequence) (index + 1) n
      where
        (lastElement : rest) = sequence
        diffElement = lastElement - index

-- >>> recaman 0
-- 0
-- >>> recaman 4
-- 2
-- >>> recaman 9
-- 21
