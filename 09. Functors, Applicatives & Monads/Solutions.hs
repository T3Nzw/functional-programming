module Solutions where

import Prelude hiding (Applicative (..), Either (..), Functor (..), Monad (..), (<$>))

changeValue :: (a -> b) -> Maybe a -> Maybe b
changeValue _ Nothing = Nothing
changeValue f (Just x) = Just (f x)

data Either a b = Left a | Right b
  deriving (Show)

safeDiv :: Double -> Double -> Either String Double
safeDiv _ 0 = Left "cannot divide by 0"
safeDiv x y = Right $ x / y

changeValueEither :: (a -> b) -> Either c a -> Either c b
changeValueEither f (Left x) = Left x
changeValueEither f (Right x) = Right (f x)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
(<$>) = fmap

instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)

instance Functor (Either c) where
  fmap :: (a -> b) -> Either c a -> Either c b
  fmap _ (Left x) = Left x
  fmap f (Right x) = Right (f x)

data List a = Nil | Cons a (List a)
  deriving (Show, Eq)

instance Semigroup (List a) where
  (<>) :: List a -> List a -> List a
  Nil <> ys = ys
  Cons x xs <> ys = Cons x $ xs <> ys

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap = map

class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  liftA2 op lhs rhs = op <$> lhs <*> rhs

instance Applicative Maybe where
  pure :: a -> Maybe a
  pure = Just

  liftA2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
  liftA2 op (Just x) (Just y) = Just $ x `op` y
  liftA2 _ _ _ = Nothing

  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  -- f :: (a -> b)
  -- x :: a
  -- (f x) :: b
  Just f <*> Just x = Just $ f x

instance Applicative (Either c) where
  pure :: a -> Either c a
  pure = Right

  (<*>) :: Either c (a -> b) -> Either c a -> Either c b
  Right f <*> Right x = Right $ f x
  Left f <*> _ = Left f
  Right f <*> Left x = Left x

instance Applicative List where
  pure :: a -> List a
  pure x = Cons x Nil

  (<*>) :: List (a -> b) -> List a -> List b
  Nil <*> _ = Nil
  Cons f fs <*> xs = (f <$> xs) <> (fs <*> xs)

instance Applicative [] where
  pure :: a -> [a]
  pure x = [x]

  (<*>) :: [a -> b] -> [a] -> [b]
  [] <*> _ = []
  (f : fs) <*> xs = f <$> xs ++ fs <*> xs

class (Applicative m) => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  lhs >> rhs = lhs >>= const rhs

instance Monad Maybe where
  return :: a -> Maybe a
  return = pure

  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing >>= f = Nothing
  Just x >>= f = f x

  (>>) :: Maybe a -> Maybe b -> Maybe b
  Nothing >> _ = Nothing
  _ >> rhs = rhs

instance Monad (Either c) where
  return :: a -> Either c a
  return = pure

  (>>=) :: Either c a -> (a -> Either c b) -> Either c b
  Left x >>= f = Left x
  Right y >>= f = f y

  (>>) :: Either c a -> Either c b -> Either c b
  Left x >> _ = Left x
  _ >> rhs = rhs

instance Monad [] where
  return :: a -> [a]
  return = pure

  (>>=) :: [a] -> (a -> [b]) -> [b]
  -- [] >>= _ = []
  -- (x:xs) >>= f = f x ++ xs >>= f
  (>>=) = flip concatMap

  (>>) :: [a] -> [b] -> [b]
  [] >> _ = []
  _ >> rhs = rhs

data Pair a b = Pair a b

class Bifunctor bf where
  bimap :: (a -> c) -> (b -> d) -> bf a b -> bf c d
  bimap f g = second g . first f

  first :: (a -> c) -> bf a b -> bf c b
  first f x = bimap f id x

  second :: (b -> d) -> bf a b -> bf a d
  second g x = bimap id g x

instance Bifunctor (,) where
  bimap :: (a -> c) -> (b -> d) -> (a, b) -> (,) c d
  bimap f g (x, y) = (f x, g y)
