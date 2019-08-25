module Main where

import Codec.Picture
import Codec.Picture.Types
import Control.DeepSeq
import System.Environment
import System.CPUTime
import Text.Printf

import Bfs
import Dfs
import AStar
import Graph
import Parser
import GraphConverter

originalPath :: String -> FilePath
originalPath name = "resources/mazes/" ++ name ++ ".png"

outPath :: String -> FilePath
outPath name = "output/mazes/" ++ name ++ ".png"

coords2Graph :: [(Int, Int)] -> Graph (Int, Int)
coords2Graph ns = Graph [(Node (a, b), [Edge (Node (a, b), Node (c, d)) | (c, d) <- ns, manhattan (a, b) (c, d) == 1]) | (a, b) <- ns]
  where
    manhattan :: (Int, Int) -> (Int, Int) -> Int
    manhattan (a, b) (c, d) = abs (a - c) + abs (b - d)

main :: IO ()
main = do
  -- Get maze name from command line
  args <- getArgs

  let mazeName = if not $ null args then head args else "MAZE00"

  -- Read image is an IO action
  mazeImage <- readImage $ originalPath mazeName

  -- Parse image to a Maze Matrix and to a graph
  let mazeMatrix = getMatrixFromImage mazeImage
  let graph = matrixToGraph mazeMatrix

  -- runAlg dfs "DFS" mazeName graph mazeMatrix
  -- runAlg bfs "BFS" mazeName graph mazeMatrix
  runAlg aStar "ASTAR" mazeName graph mazeMatrix

  return ()

runAlg alg name mazeName graph mazeMatrix = do
  let origin = head $ getNodes graph
  let target = last $ getNodes graph

  putStrLn $ "== " ++ name ++ " =="
  
  start <- getCPUTime
  let path = alg graph origin target
  end <- path `deepseq` getCPUTime

  let diff = fromIntegral (end - start) / (10^12)
  printf (name ++ " Time: %0.6f sec\n") (diff :: Double)

  let solutionMatrix = drawPath mazeMatrix path
  savePngImage (outPath (mazeName ++ "_" ++ name)) (ImageRGB8 (getImageFromMatrix solutionMatrix))

  putStrLn "========"
  return ()