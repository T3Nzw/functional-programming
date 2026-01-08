module Main where

import Control.Monad (when)
import Data.Maybe (catMaybes)
import Graph
import Map (Map)
import Queue (Queue, dequeue, enqueue, front)
import Stack (Stack, peek, pop, push)
import System.IO (hLookAhead, stdin)

import qualified Map as M
import qualified Queue as Q
import qualified Stack as S

stack :: Stack Int
stack = push 42 $ push 50 $ push 37 S.empty

-- >>> stack
-- MkStack [42,50,37]

-- >>> peek stack
-- Just 42

-- >>> pop stack
-- MkStack [50,37]

-- >>> peek $ pop $ pop stack
-- Just 37

(&) :: a -> (a -> b) -> b
-- x & f = f x
(&) = flip ($)

stack2 :: Stack Int
stack2 =
  S.empty
    & push 42
    & push 50
    & push 37

-- >>> peek Stack.empty
-- Nothing

-- >>> stack2
-- MkStack [37,50,42]

-- >>> peek stack
-- Just 42

-- >>> stack & peek
-- Just 42

queue :: Q.Queue Int
queue =
  Q.empty
    & enqueue 1
    & enqueue 5
    & enqueue 6
    & enqueue 19
    & enqueue (-1)

-- >>> queue
-- MkQueue [1,5,6,19,-1]

-- >>> dequeue queue
-- MkQueue [5,6,19,-1]

-- >>> front $ dequeue queue
-- Just 5

-- >>> dequeue $ dequeue queue
-- MkQueue [] [6,19,-1]

dequeueAll :: Queue a -> [a]
dequeueAll q = catMaybes $ go q
 where
  go :: Queue a -> [Maybe a]
  go q
    | Q.isEmpty q = []
    | otherwise = front q : go (dequeue q)

-- >>> dequeueAll queue
-- [1,5,6,19,-1]

graph :: Graph Int
graph =
  empty
    & addVertex 1
    & addVertex 2
    & addEdge 1 2

-- >>> graph
-- MkGraph (MkMap [(1,[2]),(2,[])])

-- >>> vertices graph
-- [1,2]

-- >>> adjacent 1 graph
-- Just [2]

-- >>> adjacent 2 graph
-- Just []

-- >>> adjacent 3 graph
-- Nothing

-- >>> dfs graph
-- [1,2]

-- >>> dfsState graph
-- [1,2]

graph2 :: Graph Int
graph2 =
  empty
    & addVertex 1
    & addVertex 2
    & addVertex 3
    & addVertex 4
    & addVertex 5
    & addEdge 1 2
    & addEdge 1 4
    & addEdge 2 1
    & addEdge 5 4
    & addEdge 4 1

-- >>> graph2
-- MkGraph (MkMap [(1,[4,2]),(2,[1]),(3,[]),(4,[1]),(5,[4])])

-- >>> vertices graph2
-- [1,2,3,4,5]

-- >>> adjacent 1 graph2
-- Just [4,2]

-- >>> adjacent 5 graph2
-- Just [4]

-- >>> dfs graph2
-- [1,4,2,3,5]

-- >>> dfsState graph2
-- [1,4,2,3,5]

getUntil :: [Char] -> IO String
getUntil delims = do
  c <- getChar
  if c `elem` delims
    then pure []
    else (c :) <$> getUntil delims

leadingWhitespaces :: IO ()
leadingWhitespaces = do
  c <- hLookAhead stdin
  when (c `elem` whitespaces) (getChar >> leadingWhitespaces)

-- аналогично на
-- if c `elem` whitespaces
--   then getChar >> leadingWhitespaces
--   else pure ()

whitespaces :: String
whitespaces = " \t\n"

get :: [Char] -> IO String
get delims =
  leadingWhitespaces >> getUntil delims

-- пример за четене на граф от стандартния вход
main :: IO ()
main = do
  -- за улеснение просто ще генерираме граф с върхове числата от 1 до n
  putStrLn "Enter graph info (number of vertices, number of edges):"
  numberOfVertices <- (read <$> getLine) :: IO Int
  -- в този случай (в горния също) няма нужда да пишем експлицитно,
  -- че искаме read да преобразува низа до Int, тъй като след това
  -- използваме тези две променливи във функции, които очакват
  -- аргументи от тип Int, така че GHC извежда техния тип автоматично
  numberOfEdges <- read <$> getLine

  let g' = foldr addVertex empty [1 .. numberOfVertices]

  edges <- readEdges numberOfEdges

  let g = foldr (uncurry addEdge) g' edges

  putStrLn "\nRunning DFS: " >> print (dfsState g)
 where
  readEdges :: Int -> IO [(Int, Int)]
  readEdges n
    | n <= 0 = pure []
    | otherwise = do
        u <- read <$> get whitespaces
        v <- read <$> get whitespaces
        ((u, v) :) <$> readEdges (n - 1)
