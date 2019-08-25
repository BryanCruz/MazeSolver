module AStar(aStar) where

import qualified Data.Sequence as Seq

import Graph

import Debug.Trace

manhattan :: Node (Int, Int) -> Node (Int, Int) -> Int
manhattan (Node (x1, y1)) (Node (x2, y2)) = abs (x1 - x2) + abs (y1 - y2)

--    :: Maze             -> Initial node    -> End node        -> Path
aStar :: Graph (Int, Int) -> Node (Int, Int) -> Node (Int, Int) -> [Node (Int, Int)]
aStar g st end = aStar' Seq.empty (Seq.singleton st) 0 g end

--     :: Graph            -> cost ->   Initial node  -> End node        -> Path
--aStar' :: Graph (Int, Int) ->  Int -> Node (Int, Int) -> Node (Int, Int) -> [Node (Int, Int)]
aStar' visited queue currentCost g end
  | st == end       = visited'
  | Seq.null queue' = []
  | null recursion  = 
  | otherwise       = recursion 
  where
    st        = head queue
    visited'  = visited Seq.|> st 
    recursion = aStar' visited' queue' currentCost' g 

    neighbours          = filter (`elem` visited) $ getAdjacent g st
    neighboursEvaluated = zip (map f neighbours) neighbours
    neighboursSeq       = fromList neighboursEvaluated

    f n = manhattan n end + manhattan st n

    st'     = queue `Seq.index` 0
    queue'  = sortOn fst $ (Seq.drop 1 queue) Seq.>< neighboursSeq
    currentCost' = currentCost + manhattan st st'