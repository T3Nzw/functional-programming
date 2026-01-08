module Stack
  ( Stack
  , empty
  , isEmpty
  , push
  , peek
  , pop
  , toList
  , fromList
  )
where

newtype Stack a = MkStack [a]
  deriving Show

empty :: Stack a
empty = MkStack []

isEmpty :: Stack a -> Bool
isEmpty (MkStack xs) = null xs

push :: a -> Stack a -> Stack a
push x (MkStack xs) = MkStack $ x : xs

peek :: Stack a -> Maybe a
peek (MkStack []) = Nothing
peek (MkStack (x : _)) = Just x

pop :: Stack a -> Stack a
pop s@(MkStack xs)
  | isEmpty s = s
  | otherwise = MkStack $ tail xs

toList :: Stack a -> [a]
toList (MkStack s) = s

fromList :: [a] -> Stack a
fromList = foldr push empty

instance Functor Stack where
  fmap f (MkStack stack) = MkStack $ fmap f stack

instance Foldable Stack where
  foldr op nv (MkStack stack) = foldr op nv stack
