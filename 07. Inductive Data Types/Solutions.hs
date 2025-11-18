module Solutions where

import Prelude hiding (lookup)

data Nat
  = Zero
  | Succ Nat
  deriving (Show, Eq)

succ :: Nat -> Nat
succ = Succ

pred :: Nat -> Nat
pred Zero = Zero
pred (Succ n) = n

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ $ add m n

add2 :: Nat -> Nat -> Nat
add2 Zero n = n
add2 (Succ m) n = add m (Succ n)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult (Succ m) n =
  add n $ mult m n

fromInt :: Int -> Nat
fromInt 0 = Zero
fromInt n
  | n < 0 = error "oops"
  | otherwise = Succ $ fromInt (n - 1)

toInt :: Nat -> Int
toInt Zero = 0
toInt (Succ n) = 1 + toInt n

cmp :: Nat -> Nat -> Ordering
cmp Zero Zero = EQ
cmp Zero _ = LT
cmp _ Zero = GT
cmp (Succ m) (Succ n) = cmp m n

data List a = Nil | Cons a (List a)
  deriving (Show, Eq)

isEmpty :: List a -> Bool
isEmpty Nil = True
isEmpty _ = False

headList :: List a -> Maybe a
headList Nil = Nothing
headList (Cons x _) = Just x

singleton :: a -> List a
singleton x = Cons x Nil

(+++) :: List a -> List a -> List a
Nil +++ l2 = l2
Cons x xs +++ l2 = Cons x $ xs +++ l2

reverseList :: List a -> List a
reverseList Nil = Nil
reverseList (Cons x xs) =
  reverseList xs +++ singleton x

fromList :: [a] -> List a
fromList [] = Nil
fromList (x : xs) = Cons x $ fromList xs

fromList' :: [a] -> List a
fromList' = foldr Cons Nil

toList :: List a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

-- някой от следващите пъти ще видим
-- как можем да напишем горната функция
-- с foldr :)

mapList :: (a -> b) -> List a -> List b
mapList _ Nil = Nil
mapList f (Cons x xs) = Cons (f x) $ mapList f xs

intersperse :: a -> List a -> List a
intersperse _ Nil = Nil
intersperse _ (Cons y Nil) = singleton y
intersperse x (Cons y ys) =
  Cons y $ Cons x $ intersperse x ys

-- по-подробна имплементация може да видите
-- в решенията от седмица 03
newtype Dict k v = Dict [(k, v)]
  deriving (Show)

lookup :: (Eq k) => k -> Dict k v -> Maybe v
lookup _ (Dict []) = Nothing
lookup key (Dict ((k, v) : kvps)) =
  if key == k then Just v else lookup key (Dict kvps)

data Expr a
  = Constant a
  | Variable String
  | Expr a :+: Expr a
  | Expr a :*: Expr a
  deriving (Show, Eq)

eval :: (Num a) => Dict String a -> Expr a -> Maybe a
eval _ (Constant c) = Just c
eval dict (Variable x) = lookup x dict
-- това е грозно! в някое от следващите упраженения
-- ще видим как да си спестим цялото това писане
eval dict (lhs :+: rhs) =
  let lhs' = eval dict lhs
      rhs' = eval dict rhs
   in case (lhs', rhs') of
        (Just val1, Just val2) -> Just $ val1 + val2
        _ -> Nothing
eval dict (lhs :*: rhs) =
  let lhs' = eval dict lhs
      rhs' = eval dict rhs
   in case (lhs', rhs') of
        (Just val1, Just val2) -> Just $ val1 * val2
        _ -> Nothing

dict :: Dict String Int
dict = Dict [("x", 1), ("y", 2)]

expr1 :: Expr Int
expr1 = Variable "x" :+: (Variable "y" :*: Constant 3)

expr2 :: Expr Int
expr2 = Constant 2 :*: Variable "z"

-- >>> eval dict expr1
-- Just 7

-- >>> eval dict expr2
-- Nothing

data Tree a = TNode a [Tree a]
  deriving (Show, Eq, Ord)

tree1 :: Tree Int
tree1 =
  TNode
    1
    [ TNode
        2
        [],
      TNode
        3
        [ TNode
            5
            []
        ],
      TNode
        4
        []
    ]

countNodes :: Tree a -> Int
countNodes (TNode _ xs) =
  1 + sum (map countNodes xs)

-- >>> countNodes tree1
-- 5

countLeaves :: Tree a -> Int
countLeaves (TNode _ []) = 1
countLeaves (TNode _ xs) =
  sum $ map countLeaves xs

-- >>> countLeaves tree1
-- 3

contains :: (Eq a) => a -> Tree a -> Bool
contains x (TNode y ys) =
  x == y || any (contains x) ys

-- >>> contains 5 tree1
-- True

-- >>> contains 6 tree1
-- False

flatten :: Tree a -> [a]
flatten (TNode x xs) =
  x : concatMap flatten xs

-- >>> flatten tree1
-- [1,2,3,5,4]

data BinTree a
  = Empty
  | Node a (BinTree a) (BinTree a)
  deriving (Show, Eq, Ord)

countLeavesBT :: BinTree a -> Int
countLeavesBT Empty = 0
countLeavesBT (Node _ Empty Empty) = 1
countLeavesBT (Node x l r) =
  countLeavesBT l + countLeavesBT r

height :: BinTree a -> Int
height Empty = 0
height (Node _ l r) =
  let lh = height l
      rh = height r
   in 1 + max lh rh

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT _ Empty = Empty
mapBT f (Node x l r) =
  Node (f x) (mapBT f l) (mapBT f r)

inorder :: BinTree a -> [a]
inorder Empty = []
inorder (Node x l r) =
  inorder l ++ [x] ++ inorder r

preorder :: BinTree a -> [a]
preorder Empty = []
preorder (Node x l r) =
  (x :) $ preorder l ++ preorder r

split3 :: Int -> [a] -> ([a], a, [a])
split3 _ [] = error "oops"
split3 n lst = (take n lst, lst !! n, drop (n + 1) lst)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let leq = filter (<= x) xs
      gt = filter (> x) xs
   in quicksort leq ++ [x] ++ quicksort gt

-- this is also balanced
toBST :: (Ord a) => [a] -> BinTree a
toBST lst = toBST' (length lst) $ quicksort lst
  where
    toBST' :: (Ord a) => Int -> [a] -> BinTree a
    toBST' _ [] = Empty
    toBST' len lst =
      let mid = len `div` 2
          (l, x, r) = split3 mid lst
       in Node x (toBST' mid l) (toBST' (len - mid - 1) r)

bst1 :: BinTree Int
bst1 = toBST [3, 4, 2, 1]

-- >>> bst1
-- Node 3 (Node 2 (Node 1 Empty Empty) Empty) (Node 4 Empty Empty)

bst2 :: BinTree Int
bst2 = toBST [6, 4, 2, 5, 1, 3]

-- >>> bst2
-- Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 6 (Node 5 Empty Empty) Empty)

-- >>> countLeavesBT bst1
-- 2

-- >>> countLeavesBT bst2
-- 3

-- >>> height bst1
-- 3

-- >>> mapBT (`mod` 3) bst1
-- Node 0 (Node 2 (Node 1 Empty Empty) Empty) (Node 1 Empty Empty)

-- >>> inorder bst1
-- [1,2,3,4]

-- >>> inorder bst2
-- [1,2,3,4,5,6]

type Interval a = (a, a)

in' :: (Ord a) => a -> Interval a -> Bool
x `in'` (lower, upper) = lower <= x && x < upper

isBST :: (Ord a, Bounded a) => BinTree a -> Bool
isBST = helper (minBound, maxBound)
  where
    helper :: (Ord a, Bounded a) => Interval a -> BinTree a -> Bool
    helper _ Empty = True
    helper interval@(lower, upper) (Node x l r) =
      x `in'` interval
        && helper (lower, x) l
        && helper (x, upper) r

-- >>> isBST bst1
-- True

-- >>> isBST bst2
-- True

-- >>> isBST $ Node 1 Empty (leaf (2 :: Int))
-- True

-- >>> isBST $ Node 1 (leaf (2 :: Int)) Empty
-- False

isBalanced :: BinTree a -> Bool
isBalanced Empty = True
isBalanced (Node x l r) =
  abs (height l - height r) <= 1
    && isBalanced l
    && isBalanced r

isBalancedBST :: (Ord a, Bounded a) => BinTree a -> Bool
isBalancedBST tree = isBalanced tree && isBST tree

type Product = Int

type Length = Int

-- в задачата се иска само дължината,
-- но с цел по-лесна проверка ще връщаме
-- и произведението. бихме могли да връщаме
-- и самия път
minProdMaxLen :: BinTree Int -> (Product, Length)
minProdMaxLen = helper 1 0
  where
    min' :: (Product, Length) -> (Product, Length) -> (Product, Length)
    min' lhs@(prod1, len1) rhs@(prod2, len2)
      | prod1 == prod2 = if len1 > len2 then lhs else rhs
      | prod1 < prod2 = lhs
      | otherwise = rhs

    helper :: Product -> Length -> BinTree Int -> (Product, Length)
    helper prod len Empty = (prod, len)
    helper prod len (Node x l r) =
      let newProd = if odd x then prod * x else prod
          res = (newProd, len)
          lres = helper newProd (len + 1) l
          rres = helper newProd (len + 1) r
       in min' (min' lres rres) res

leaf :: a -> BinTree a
leaf x = Node x Empty Empty

btree3 :: BinTree Int
btree3 =
  Node
    1
    ( Node
        2
        (leaf $ -1)
        (leaf 1)
    )
    ( Node
        2
        (leaf 3)
        (leaf $ -4)
    )

-- >>> minProdMaxLen btree3
-- (-1,3)
