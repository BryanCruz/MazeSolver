module Main where

import Graph
import Parser

mazeName = "MAZE01"

coords2Graph :: [(Int, Int)] -> Graph (Int, Int)
coords2Graph ns = Graph [(Node (a, b), [Edge (Node (a, b), Node (c, d)) | (c, d) <- ns, manhattan (a, b) (c, d) == 1]) | (a, b) <- ns]
  where
    manhattan :: (Int, Int) -> (Int, Int) -> Int
    manhattan (a, b) (c, d) = abs (a - c) + abs (b - d)

main :: IO ()
main = do
  mazeMatrix <- getMazeMatrix mazeName
  print mazeMatrix
