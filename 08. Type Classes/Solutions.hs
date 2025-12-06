-- това тук може да го игнорирате
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Solutions where

-- импровизация на това, което писахме на семинара, защото не си пратих файла...
-- + решенията

import Data.Char (chr, ord)
import Prelude hiding (Functor (..), Monoid (..), Semigroup (..))

class Eq' a where
  (===) :: a -> a -> Bool
  x === y = not $ x /== y

  (/==) :: a -> a -> Bool
  x /== y = not $ x === y

instance Eq' Int where
  x === y = x == y

instance Eq' Char where
  x === y = ord x === ord y

instance (Eq' a) => Eq' [a] where
  [] === [] = True
  (x : xs) === (y : ys) = x === y && xs === ys
  _ === _ = False

class Show' a where
  show' :: a -> String

instance Show' Int where
  show' 0 = "0"
  show' n = go n
    where
      go n'
        | n' == 0 = ""
        | n' < 0 = "-" ++ show' (abs n')
        | otherwise = go (n' `div` 10) ++ [chr $ n' `mod` 10 + ord '0']

-- String == [Char]
instance Show' String where
  show' str = "\"" ++ str ++ "\""

data Maybe' a = Nothing' | Just' a

instance (Eq' a) => Eq' (Maybe' a) where
  Nothing' === Nothing' = True
  Just' x === Just' y = x === y
  _ === _ = False

class Sizeable a where
  size :: a -> Int
  size _ = 1

instance Sizeable Int where
  size _ = 4

instance (Sizeable a) => Sizeable (Maybe a) where
  size Nothing = 0
  size (Just x) = 1 + size x

instance (Sizeable a) => Sizeable [a] where
  size xs = sum $ map size xs

newtype Down a = Down a
  deriving (Show, Eq)

instance (Ord a) => Ord (Down a) where
  Down x <= Down y = x >= y

data List a = Nil | Cons a (List a)

instance (Show a) => Show (List a) where
  show xs = "(" ++ show' xs ++ ")"
    where
      show' :: (Show a) => List a -> String
      show' Nil = ""
      show' (Cons x Nil) = show x
      show' (Cons x xs) = show x ++ "," ++ show' xs

data NonEmpty a = a :| [a] deriving (Show, Eq)

class Semigroup a where
  (<>) :: a -> a -> a

  sconcat :: NonEmpty a -> a
  sconcat (x :| xs) = foldl (<>) x xs

instance Semigroup (List a) where
  Nil <> l2 = l2
  (Cons x xs) <> l2 = Cons x $ xs <> l2

class (Semigroup a) => Monoid a where
  mempty :: a

  mappend :: a -> a -> a
  mappend = (<>)

  mconcat :: [a] -> a
  -- може и foldl; може и <> вместo mappend
  mconcat = foldr mappend mempty

instance Monoid (List a) where
  mempty = Nil

-- няма нужда да дефинираме останалите методи,
-- тъй като вече сме имплементирали <> за List,
-- а mconcat се получава на база mempty и mappend

instance (Eq a) => Eq (List a) where
  Nil == Nil = True
  Cons x xs == Cons y ys = x == y && xs == ys
  _ == _ = False

instance (Eq' a) => Eq' (List a) where
  Nil === Nil = True
  Cons x xs === Cons y ys = x === y && xs === ys
  _ === _ = False

instance (Ord a) => Ord (List a) where
  Nil <= _ = True
  _ <= Nil = False
  Cons x xs <= Cons y ys = x <= y && xs <= ys

-- това няма да го скриваме и предефинираме,
-- защото много от горните функции идват от Foldable
class Foldable' c where
  foldr' :: (a -> b -> b) -> b -> c a -> b

instance Foldable' List where
  foldr' _ nv Nil = nv
  foldr' op nv (Cons x xs) = x `op` foldr' op nv xs

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

data BinTree a = Empty | Node a (BinTree a) (BinTree a)
  deriving (Show, Eq)

instance Functor BinTree where
  fmap _ Empty = Empty
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

instance Foldable BinTree where
  foldr _ nv Empty = nv
  foldr op nv (Node x l r) =
    foldr op (x `op` foldr op nv r) l

class Brzozowski r where
  nullable :: r a -> Bool
  derivative :: (Eq a) => a -> r a -> r a

data DFA s a = DFA
  { _start :: s,
    _delta :: s -> a -> s,
    _accept :: [s]
  }

data Regex a
  = REmpty
  | Epsilon
  | Atom a
  | KStar (Regex a)
  | Regex a :+ Regex a
  | Regex a :. Regex a
  deriving (Show, Eq)

infixl 4 :+

infixl 5 :.

reconcat :: Regex a -> Regex a -> Regex a
reconcat REmpty _ = REmpty
reconcat _ REmpty = REmpty
reconcat Epsilon re = re
reconcat re Epsilon = re
reconcat re1 re2 = re1 :. re2

-- така няма да опростим регулярния израз,
-- това трябва да се направи рекурсивно по
-- структурата на Regex a
reunion :: Regex a -> Regex a -> Regex a
reunion REmpty re = re
reunion re REmpty = re
reunion re1 re2 = re1 :+ re2

instance Brzozowski Regex where
  nullable Epsilon = True
  nullable (Atom _) = False
  nullable (KStar _) = True
  nullable (lhs :+ rhs) = nullable lhs || nullable rhs
  nullable (lhs :. rhs) = nullable lhs && nullable rhs

  derivative _ Epsilon = Epsilon
  derivative x (Atom y) =
    if x == y then Epsilon else REmpty
  derivative x (KStar re) =
    reconcat (derivative x re) (KStar re)
  derivative x (lhs :+ rhs) =
    reunion (derivative x lhs) (derivative x rhs)
  derivative x (lhs :. rhs) =
    reconcat (derivative x lhs) rhs

instance (Eq s) => Brzozowski (DFA s) where
  nullable (DFA start _ final) = start `elem` final

  derivative x (DFA start delta final) = DFA (delta start x) delta final

class Stream s where
  empty :: s a
  cons :: a -> s a -> s a
  uncons :: s a -> Maybe (a, s a)

instance Stream [] where
  empty = []
  cons = (:)
  uncons xs =
    case xs of
      [] -> Nothing
      h : t -> Just (h, t)

instance Stream List where
  empty = Nil
  cons = Cons
  uncons xs =
    case xs of
      Nil -> Nothing
      Cons h t -> Just (h, t)

-- тази имплементация използва т.нар. типови фамилии,
-- с които няма да се занимаваме в този курс
class Stream' s where
  type Element s
  empty' :: s
  cons' :: Element s -> s -> s
  uncons' :: s -> Maybe (Element s, s)

instance Stream' [a] where
  type Element [a] = a
  empty' = []
  cons' = (:)
  uncons' xs =
    case xs of
      [] -> Nothing
      (h : t) -> Just (h, t)

instance Stream' (List a) where
  type Element (List a) = a
  empty' = Nil
  cons' = Cons
  uncons' xs =
    case xs of
      Nil -> Nothing
      Cons h t -> Just (h, t)

-- тази имплементация използва типови класове с множество параметри
-- и функционални зависимости (s -> el означава, че изборът на s
-- еднозначно определя какво е el, т.е. инекция)
class Stream'' s el | s -> el where
  empty'' :: s
  cons'' :: el -> s -> s
  uncons'' :: s -> Maybe (el, s)

instance Stream'' [a] a where
  empty'' = []
  cons'' = (:)
  uncons'' xs = case xs of
    [] -> Nothing
    h : t -> Just (h, t)

instance Stream'' (List a) a where
  empty'' = Nil
  cons'' = Cons
  uncons'' xs = case xs of
    Nil -> Nothing
    Cons h t -> Just (h, t)
