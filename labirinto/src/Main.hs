module Main where

import Parser

mazeName = "MAZE01"

main :: IO ()
main = do
  mazeMatrix <- getMazeMatrix mazeName
  print $ mazeMatrix
