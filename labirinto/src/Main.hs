module Main where

import Graph
import Parser

mazeName = "MAZE01"

main :: IO ()
main = do
  mazeMatrix <- getMazeMatrix mazeName
  print $ mazeMatrix
