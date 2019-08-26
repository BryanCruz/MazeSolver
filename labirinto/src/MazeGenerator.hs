module MazeGenerator(
  generateMaze,
  Size (XS, S, M, L, XL)
) where

import Graph
import GraphConverter
import Data.Maybe
import qualified Data.List  as List
import System.Random
import qualified Data.Map as Map

import Debug.Trace

data Size = XS | S | M | L | XL deriving (Enum, Show, Eq, Read)
type UnionFind a = Map.Map a (Int, a)

newUnionFind :: Graph (Int, Int) -> UnionFind (Int, Int)
newUnionFind g = foldr (\(Node n) m -> Map.insert n (1, n) m) Map.empty $ getNodes g

find :: UnionFind (Int, Int) -> (Int, Int) -> (Int, (Int, Int))
find uf x | snd parent == x = parent
          | otherwise       = find uf x'
  where
    x' = snd $ fromMaybe (0, (0, 0)) $ Map.lookup x uf
    parent = fromMaybe (0, (0, 0)) $ Map.lookup x uf

union :: UnionFind (Int, Int) -> (Int, Int) -> (Int, Int) -> UnionFind (Int, Int)
union uf a b | ida == idb = uf
             | sza <= szb = Map.adjust (\(x, y) -> (x, idb)) a $ Map.adjust (\(x, y) -> (x + sza, y)) b uf
             | otherwise  = Map.adjust (\(x, y) -> (x, ida)) b $ Map.adjust (\(x, y) -> (x + szb, y)) a uf
  where
    (sza, ida) = find uf a
    (szb, idb) = find uf b

sizeToPxs :: Size -> Int
sizeToPxs sz = sizes !! fromEnum sz
  where
    sizes = [11, 25, 45, 75, 91]

resizeGraph :: Graph (Int, Int) -> Graph (Int, Int)
resizeGraph (Graph ns) = graphInit $ (0, 1) : (x, x + 1) : nodeList
   where
    x = maximum $ map snd nodeList
    nodeList = List.nub (averages ++ (map (double . getId)  (getNodes (Graph ns))))
    averages = concat [map average es | (n, es) <- ns]
    average (Edge (Node (x1, y1), Node (x2, y2))) = (x1 + x2 + 1, y1 + y2 + 1)
    double (x, y) = (2 * x + 1, 2 * y + 1)

newSquareGraph :: Int -> Graph (Int, Int)
newSquareGraph n = Graph [(Node (x, y), []) | x <- [0..(n - 1)], y <- [0..(n - 1)]]

directions :: [(Int, Int) -> (Int, Int)]
directions = [up, dwn, lft, rgt]
  where
    up  = \(x,y) -> (x, y-1)
    dwn = \(x,y) -> (x, y+1)
    lft = \(x,y) -> (x-1, y)
    rgt = \(x,y) -> (x+1, y)

shuffle :: [a] -> StdGen -> [a] 
shuffle xs rd = shuffle' xs (length xs) rd  
  where
    shuffle' _  0 _  = []
    shuffle' xs n rd = x : shuffle' (left ++ right) (n - 1) rd'
      where
        left = take pos xs
        right = drop (pos + 1) xs
        x = xs !! pos
        (pos, rd') = randomR (0, n - 1) rd

generateMaze :: Size -> StdGen -> Graph (Int, Int)
generateMaze sz rd = resizeGraph newG
  where
    newG = generateMaze' g (newUnionFind g) (n * n) possibleEdges
    n = sizeToPxs sz `div` 2
    g = newSquareGraph n

    possibleEdges :: [Edge (Int, Int)]
    possibleEdges = shuffle (getPossibleEdges n) rd

    generateMaze' g _  1 _                          = g
    generateMaze' g uf n (Edge (Node a, Node b):es) = g'
      where
        g' | find uf a == find uf b = generateMaze' g uf n es 
           | otherwise              = generateMaze' (addEdge g (Node a) (Node b)) (union uf a b) (n - 1) es

getPossibleEdges :: Int -> [Edge (Int, Int)]
getPossibleEdges n = concatMap (\(x,y) -> getEdges (x,y)) nodes
  where
    nodes :: [(Int, Int)]
    nodes = [(x,y) | x <- [0..n], y <- [0..n]]

    getEdges :: (Int, Int) -> [Edge (Int, Int)]
    getEdges node = map (\node' -> Edge (Node node, Node node')) (getNeighbors node)

    getNeighbors :: (Int, Int) -> [(Int, Int)] 
    getNeighbors node = filter isValid $ map (\dir -> dir node) directions

    isValid :: (Int, Int) -> Bool
    isValid (x,y) = x >= 0 && x < n && y >= 0 && y < n
