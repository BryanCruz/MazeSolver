module Main where

import Codec.Picture
import Codec.Picture.Types

import Text.Printf
import System.Random
import System.CPUTime
import Control.DeepSeq
import System.Environment

import Bfs
import Dfs
import AStar
import Graph
import Parser
import GraphConverter
import qualified MazeGenerator as MazeGen

originalPath :: String -> FilePath
originalPath name = "resources/mazes/" ++ name ++ ".png"

outPath :: String -> FilePath
outPath name = "output/mazes/" ++ name ++ ".png"

coords2Graph :: [(Int, Int)] -> Graph (Int, Int)
coords2Graph ns = Graph [(Node (a, b), [Edge (Node (a, b), Node (c, d)) | (c, d) <- ns, manhattan (a, b) (c, d) == 1]) | (a, b) <- ns]
  where
    manhattan :: (Int, Int) -> (Int, Int) -> Int
    manhattan (a, b) (c, d) = abs (a - c) + abs (b - d)

newMaze size = do
  randomX <- randomRIO (0, 100000000)
  let s = read size
  let g = MazeGen.generateMaze s $ mkStdGen randomX
  let m = graphToMatrix g
  mapM_ print m 
  savePngImage (outPath ("randomMaze")) (ImageRGB8 (getImageFromMatrix m))

main :: IO ()
main = do
  -- Get maze name from command line
  args <- getArgs

  let mazeSize = (if (length args) >= 2 then (args !! 1) else "XS")
  if   head args == "generate"
  then newMaze mazeSize
  else do
    let mazeName = if not $ null args then head args else "MAZE00"

    -- Read image is an IO action
    mazeImage <- readImage $ originalPath mazeName

    -- Parse image to a Maze Matrix and to a graph
    let mazeMatrix = getMatrixFromImage mazeImage
    let graph = matrixToGraph mazeMatrix

    runAlg aStar "ASTAR" mazeName graph mazeMatrix
    runAlg bfs "BFS" mazeName graph mazeMatrix
    runAlg dfs "DFS" mazeName graph mazeMatrix
  
    return ()

runAlg alg name mazeName graph mazeMatrix = do
  let origin = head $ getNodes graph
  let target = last $ getNodes graph

  putStrLn $ "== " ++ name ++ " =="
  
  start <- getCPUTime
  let (path, visitedNodes) = alg graph origin target
  end <- path `deepseq` (visitedNodes `deepseq` getCPUTime)

  print ("Nodes Visited " ++ (show (length visitedNodes)))
  let diff = fromIntegral (end - start) / (10^12)
  printf "Time: %0.6f sec\n" (diff :: Double)

  let colorMapMatrix = drawColorMap mazeMatrix visitedNodes
  let solutionMatrix = drawPath colorMapMatrix path
  savePngImage (outPath (mazeName ++ "_" ++ name)) (ImageRGB8 (getImageFromMatrix solutionMatrix))

  putStrLn "========"
  return ()