module AStar(aStar) where

import Graph
import Data.List

import Debug.Trace
import Data.Foldable (toList)

manhattan :: Node (Int, Int) -> Node (Int, Int) -> Int
manhattan (Node (x1, y1)) (Node (x2, y2)) = abs (x1 - x2) + abs (y1 - y2)

--    :: Maze             -> Initial node    -> End node        -> Path
aStar :: Graph (Int, Int) -> Node (Int, Int) -> Node (Int, Int) -> [Node (Int, Int)]
aStar g st end = reverse $ aStar' [] [(0, st)] 0 g end

--     :: Graph            -> cost ->   Initial node  -> End node        -> Path
--aStar' :: Graph (Int, Int) ->  Int -> Node (Int, Int) -> Node (Int, Int) -> [Node (Int, Int)]
aStar' visited queue currentCost g end
  | st == end   = visited'
  | null queue' = []
  | otherwise   = recursion 
    where
      st        = snd $ head queue
      visited'  = st : visited

      recursion = aStar' visited' queue' currentCost' g end

      neighbours          = filter (\n -> n `notElem` visited' && n `notElem` queueNodes) $ getAdjacent g st
      neighboursEvaluated = zip (map f neighbours) neighbours
      queueNodes          = map snd $ tail queue
      queueReevaluated    = zip (map f queueNodes) queueNodes
      queue'              = sortOn fst $ zipWith (curry getMinor) queueReevaluated queue ++ neighboursEvaluated
        where
          getMinor ((v, n), (v', _)) = (min v v', n)

      f n = currentCost' + manhattan n end
      currentCost' = currentCost + 1