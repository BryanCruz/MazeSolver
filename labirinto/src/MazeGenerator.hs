module MazeGenerator where

import Graph
import System.Random


-- recursive backtracking, primeiro algoritmo: https://www.youtube.com/watch?v=6kv5HKPB1XU
-- https://github.com/jsmolka/maze/tree/master/maze

data Size = XS | S | M | L | XL deriving (Enum, Show, Eq, Read)

sizeToPxs :: Size -> Int
sizeToPxs sz = sizes !! fromEnum sz
  where
    sizes = [11, 25, 45, 75, 91]

generateMaze :: Size -> Graph (Int, Int)
generateMaze sz = resizeGraph $ newMaze $ sizeToPxs sz `div` 2

resizeGraph :: Graph (Int, Int) -> Graph (Int, Int)
resizeGraph (Graph ns) = Graph newNs
  where 
    newNs                                  = [(newNode n, map middleNode es) | (n, es) <- ns]
    newNode (Node (x, y))                  = Node (2 * x, 2 * y)
    middleNode (Edge (n1, n2))             = Edge (newNode n1, average (newNode n1) (newNode n2))
    average (Node (x1, y1), Node (x2, y2)) = Node ((x1 + x2) `div` 2, (y1 + y2) `div` 2)

newMaze :: Int -> Graph (Int, Int)
newMaze n = newMaze' n [(0, 0)] []
  where
    neighbors (x, y) = filter ((not . (`elem` visited)) . (isValid n)) [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]
    shuffled = shuffle neighbors
    isValid n (x, y) = x >= 0 && x < n && y >= 0 && y < n
    newMaze' _ [] _ = undefined
    newMaze' n ((x, y):q) visited = 