module State
  ( State (runState)
  , get
  , put
  , modify
  , evalState
  )
where

import Data.Bifunctor (first)

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap f (State state) = State $ first f . state

instance Applicative (State s) where
  pure x = State $ \s -> (x, s)

  State sf <*> State state =
    State $ \s ->
      let (f, s') = sf s
          (x, s'') = state s'
       in (f x, s'')

instance Monad (State s) where
  return = pure

  State state >>= f =
    State $ \s ->
      let (x, s') = state s
          State state2 = f x
       in state2 s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \s -> ((), s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

evalState :: State s a -> s -> a
evalState state initial = fst $ runState state initial
