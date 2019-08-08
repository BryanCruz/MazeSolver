module GraphConverter where

import Graph

graphToMatrix :: Graph (Int, Int) -> [[Int]]
graphToMatrix g = [[if (x, y) `elem` nodePos then 0 else 1 | y <- [minY..maxY]] | x <- [minX..maxX]]
  where
    nodes = getNodes g
    nodePos = [t | (Node t) <- nodes]
    (minX, maxX, minY, maxY) = getBounds nodePos (1000000000, 0, 1000000000, 0)
    getBounds :: [(Int, Int)] -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
    getBounds [] bs = bs
    getBounds ((x, y):xs) (xMin, xMax, yMin, yMax) = getBounds xs (min x xMin, max x xMax, min y yMin, max y yMax)


matrixToGraph :: [[Int]] -> Graph (Int, Int)
matrixToGraph xss = graphInit $ concat [[(i, j) | (j, y) <- xs, y == 0] | (i, xs) <- indexed]
  where
    indexed = [(fst x, zip [0..] $ snd x) | x <- indexLines]
    indexLines = zip [0..] xss

{- [(0, [(0, 1) (1, 0) (2, 0)]),
    (1, [0 1 0]),
    (2, [0 0 1])] -}

graphInit :: [(Int, Int)] -> Graph (Int, Int)
graphInit xs = Graph [(Node x, [Edge (Node x, Node y) | y <- xs, manhattan x y == 1]) | x <- xs]
    where
      manhattan (a, b) (c, d) = abs (a - c) + abs (b - d)