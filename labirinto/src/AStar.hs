module AStar(aStar) where

import Graph
import Data.List

import Data.Foldable (toList)

manhattan :: Node (Int, Int) -> Node (Int, Int) -> Int
manhattan (Node (x1, y1)) (Node (x2, y2)) = abs (x1 - x2) + abs (y1 - y2)

--    :: Maze             -> Initial node    -> End node        -> Path and Visited Nodes
aStar :: Graph (Int, Int) -> Node (Int, Int) -> Node (Int, Int) -> ([Node (Int, Int)], [Node (Int, Int)])
aStar g st end = (path, nodesVisited)
  where
    result       = aStar' [] [(0, (st, st))] 0 g end
    path         = buildPath result end
    nodesVisited = getVisited result

buildPath :: Eq a => [(Node a, Node a)] -> Node a -> [Node a]
buildPath [] _ = []
buildPath (n:ns) pred
  | uncurry (==) n = [fst n]
  | fst n == pred  = buildPath ns (snd n)++[pred]
  | otherwise      = buildPath ns pred

getVisited ns = reverse [fst n | n <- ns]

aStar' visited queue currentCost g end
  | st == end   = visited'
  | null queue' = []
  | otherwise   = recursion 
    where
      st        = fst $ snd $ head queue
      pred      = snd $ snd $ head queue
      visited'  = (st, pred) : visited

      recursion = aStar' visited' queue' currentCost' g end

      neighbours          = map (\n -> (n, st)) $ filter (\n -> n `notElem` (map fst visited') && n `notElem` (map fst queueNodes)) $ getAdjacent g st
      neighboursEvaluated = zip (map f neighbours) neighbours
      queueNodes          = map snd $ tail queue
      queueReevaluated    = zip (map f queueNodes) queueNodes
      queue'              = sortOn fst $ zipWith (curry getMinor) queueReevaluated queue ++ neighboursEvaluated
        where
          getMinor ((v, (n, p)), (v', (_, p'))) = if (v <= v') then (v, (n, p)) else (v', (n, p'))

      heuristicsMultiplier = 1000
      f (n, _) = currentCost' + 1000 * manhattan n end
      currentCost' = currentCost + 1