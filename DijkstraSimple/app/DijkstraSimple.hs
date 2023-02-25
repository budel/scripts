module DijkstraSimple where

{- required packages:
   containers, unordered-containers, hashable
-}

import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import qualified Data.Heap as H
import Data.Heap (MinPrioHeap)
import qualified Data.HashSet as HS
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)

data Distance a = Dist a | Infinity
  deriving (Show, Eq)

instance (Ord a) => Ord (Distance a) where
  Infinity <= Infinity = True
  Infinity <= Dist _ = False
  Dist _ <= Infinity = True
  Dist x <= Dist y = x <= y

addDist :: (Num a) => Distance a -> Distance a -> Distance a
addDist (Dist x) (Dist y) = Dist (x + y)
addDist _ _ = Infinity

(!??) :: (Hashable k, Eq k) => HashMap k (Distance d) -> k -> Distance d
(!??) distMap key = fromMaybe Infinity (HM.lookup key distMap)

newtype Graph = Graph
   { edges :: HashMap String [(String, Int)] }

data DijkstraState = DijkstraState
  { visitedSet :: HashSet String
  , distanceMap :: HashMap String (Distance Int)
  , nodeQueue :: MinPrioHeap (Distance Int) String
  }


findShortestDistance :: Graph -> String -> String -> Distance Int
findShortestDistance graph src dest = processQueue initialState !?? dest
  where
    initialVisited = HS.empty
    initialDistances = HM.singleton src (Dist 0)
    initialQueue = H.fromList [(Dist 0, src)]
    initialState = DijkstraState initialVisited initialDistances initialQueue

    processQueue :: DijkstraState -> HashMap String (Distance Int)
    processQueue ds@(DijkstraState v0 d0 q0) = case H.view q0 of
      Nothing -> d0
      Just ((_, node), q1) -> if node == dest then d0
        else if HS.member node v0 then processQueue (ds {nodeQueue = q1})
        else
          -- Update the visited set
          let v1 = HS.insert node v0
          -- Get all unvisited neighbors of our current node
              allNeighbors = fromMaybe [] (HM.lookup node (edges graph))
              unvisitedNeighbors = filter (\(n, _) -> not (HS.member n v1)) allNeighbors
          -- Fold each neighbor and recursively process the queue
          in  processQueue $ foldl (foldNeighbor node) (DijkstraState v1 d0 q1) unvisitedNeighbors
    foldNeighbor current ds@(DijkstraState v1 d0 q1) (neighborNode, cost) =
      let altDistance = addDist (d0 !?? current) (Dist cost)
      in  if altDistance < d0 !?? neighborNode
            then DijkstraState v1 (HM.insert neighborNode altDistance d0) (H.insert (altDistance, neighborNode) q1)
            else ds

