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

import Debug.Trace

originalPath :: String -> FilePath
originalPath name = "resources/mazes/" ++ name ++ ".png"

outPath :: String -> FilePath
outPath name = "output/mazes/" ++ name ++ ".png"

main :: IO ()
main = do
  -- Get args from command line
  args <- getArgs
  
  let arg1 = if not $ null args
             then head args
             else "MAZE00"

  if arg1 == "generate"
  then do
    let mazeSize = if length args > 1 then args !! 1 else "M"
    (mazeMatrix, graph) <- newMaze mazeSize
    main' "randomMaze" graph mazeMatrix
  else do
    let imageName = arg1
    img <- readImage $ originalPath imageName
    let mazeMatrix = getMatrixFromImage img
    let graph = matrixToGraph mazeMatrix
    main' imageName graph mazeMatrix

  return ()

main' name graph mazeMatrix = do
  runAlg bfs "BFS" name graph mazeMatrix
  runAlg dfs "DFS" name graph mazeMatrix
  runAlg aStar "ASTAR" name graph mazeMatrix

newMaze size = do
  randomX <- randomRIO (0, 100000000)
  let s = read size
  let g = MazeGen.generateMaze s $ mkStdGen randomX
  let m = graphToMatrix g
  savePngImage (outPath "randomMaze") (ImageRGB8 (getImageFromMatrix m))
  return (m, g)

runAlg alg name mazeName graph mazeMatrix = do
  let origin = head $ getNodes graph
  let target = last $ getNodes graph

  let opener = "== " ++ name ++ " =="
  putStrLn opener
  
  start <- getCPUTime
  let (path, visitedNodes) = alg graph origin target
  end <- path `deepseq` (visitedNodes `deepseq` getCPUTime)

  print ("Nodes Visited: " ++ (show (length visitedNodes)))
  let diff = fromIntegral (end - start) / (10^12)
  printf "Time: %0.6f sec\n" (diff :: Double)

  let colorMapMatrix = drawColorMap mazeMatrix visitedNodes
  let solutionMatrix = drawPath colorMapMatrix path
  savePngImage (outPath (mazeName ++ "_" ++ name)) (ImageRGB8 (getImageFromMatrix solutionMatrix))

  putStrLn $ (replicate (length opener) '=')
  return ()