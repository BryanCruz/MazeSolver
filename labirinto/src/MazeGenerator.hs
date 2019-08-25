module MazeGenerator where

import Graph
import System.Random

generateMaze :: Int -> StdGen -> Graph
generateMaze n rd = generateMaze'
  where
    generateMaze' = undefined