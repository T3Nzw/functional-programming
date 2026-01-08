module Graph
  ( Graph
  , empty
  , addVertex
  , addEdge
  , vertices
  , adjacent
  , dfs
  , dfsState
  )
where

import Data.Function ((&))
import Map (Map)
import State

import qualified Map as M

newtype Graph v = MkGraph (Map v [v])
  deriving Show

empty :: Graph v
empty = MkGraph M.empty

addVertex :: Ord v => v -> Graph v -> Graph v
addVertex vertex (MkGraph g) =
  MkGraph $ case M.lookup vertex g of
    Nothing -> M.insert vertex [] g
    Just _ -> g

addEdge :: Ord v => v -> v -> Graph v -> Graph v
addEdge u v (MkGraph g) =
  MkGraph $ case (M.lookup u g, M.lookup v g) of
    (Just adj, Just _)
      | v `elem` adj -> g
      | otherwise -> M.insert u (v : adj) g
    _ -> g

vertices :: Graph v -> [v]
vertices (MkGraph g) = M.keys g

adjacent :: Ord v => v -> Graph v -> Maybe [v]
adjacent v (MkGraph g) = M.lookup v g

type Visited v = [v]

dfsFrom :: Ord v => v -> Graph v -> Visited v -> (Visited v, [v])
dfsFrom u g visited
  | u `elem` visited = (visited, [])
  | otherwise =
      case adjacent u g of
        Nothing -> (visited, [])
        Just adj ->
          foldl
            ( \(visited, vertices) child ->
                let (visited', vertices') = dfsFrom child g visited
                 in (visited', vertices ++ vertices')
            )
            (u : visited, [u])
            adj

dfs :: Ord v => Graph v -> [v]
dfs g =
  snd
    $ foldl
      ( \(visited, vertices) vertex ->
          let (visited', vertices') = dfsFrom vertex g visited
           in (visited', vertices ++ vertices')
      )
      ([], [])
    $ vertices g

-- използвайки state монадата

dfsFromState :: Ord v => v -> Graph v -> State (Visited v) [v]
dfsFromState vertex g = do
  visited <- get

  if vertex `elem` visited
    then pure []
    else case adjacent vertex g of
      Nothing -> pure []
      Just adj -> do
        modify (vertex :)
        (vertex :) . concat <$> mapM (`dfsFromState` g) adj

dfsState :: Ord v => Graph v -> [v]
dfsState g = evalState (concat <$> mapM (`dfsFromState` g) (vertices g)) []
