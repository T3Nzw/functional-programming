module Queue
  ( Queue
  , empty
  , isEmpty
  , enqueue
  , dequeue
  , front
  , toList
  , fromList
  )
where

-- import Stack
-- data Queue a = MkQueue (Stack a) (Stack a)

data Queue a = MkQueue [a] [a]

-- това е с цел по-добра четимост, т.е. форматираме
-- опашката точно по начина, по който бихме искали да изглежда тя
instance Show a => Show (Queue a) where
  show q = "MkQueue " ++ show (toList q)

empty :: Queue a
empty = MkQueue [] []

isEmpty :: Queue a -> Bool
isEmpty (MkQueue [] []) = True
isEmpty (MkQueue{}) = False

enqueue :: a -> Queue a -> Queue a
enqueue x (MkQueue [] []) = MkQueue [] [x]
enqueue x (MkQueue xs ys) = MkQueue (x : xs) ys

dequeue :: Queue a -> Queue a
dequeue q@(MkQueue [] []) = q
dequeue (MkQueue xs []) = MkQueue [] $ let (y : ys) = reverse xs in ys
dequeue (MkQueue xs (y : ys)) = MkQueue xs ys

front :: Queue a -> Maybe a
front (MkQueue [] []) = Nothing
front (MkQueue xs []) = Just $ let (y : _) = reverse xs in y
front (MkQueue _ (y : _)) = Just y

toList :: Queue a -> [a]
toList (MkQueue s1 s2) = s1 ++ reverse s2

fromList :: [a] -> Queue a
fromList = foldr enqueue empty

instance Functor Queue where
  fmap f (MkQueue s1 s2) = MkQueue (fmap f s1) (fmap f s2)

instance Foldable Queue where
  foldr op nv q = foldr op nv $ toList q
