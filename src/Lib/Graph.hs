module Lib.Graph where

import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet

type Node = Int
type Graph = IntMap [Node]

toNeighbors :: Node -> Graph -> [Node]
toNeighbors node graph =
    case IntMap.lookup node graph of
        Nothing        -> []
        Just neighbors -> neighbors

bfs :: Node -> Graph -> [[Node]]
bfs initial graph = go (IntSet.singleton initial) [initial] where
    go :: IntSet -> [Node] -> [[Node]]
    go visited latests =
        let nextsCandidates = concatMap (\latest -> toNeighbors latest graph) latests
            nextsSet = IntSet.fromList nextsCandidates `IntSet.difference` visited
            nextsList = IntSet.toList nextsSet
        in if null nextsList
              then []
              else nextsList : go (visited `IntSet.union` nextsSet) nextsList

edgesToGraph :: [(Int, Int)] -> Graph
edgesToGraph =
    IntMap.fromListWith (++) . concatMap (\(start, end) -> [(end, [start]), (start, [end])])

-- >>> bfs 3 $ edgesToGraph [(1,2), (1,3), (3,5), (5,7), (2,7), (7,9)]
-- [[1,5],[2,7],[9]]

{-

   1
  2 3
 /   5
  \ /
   7
   9
-}
