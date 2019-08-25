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

  print "== BFS =="

  start <- getCPUTime
  let bfsPath = bfs graph (head $ getNodes graph) (last $ getNodes graph)
  end  <- bfsPath `deepseq` getCPUTime
  let diff = fromIntegral (end - start) / (10^12)
  printf "BFS Time: %0.3f sec\n" (diff :: Double)

  let bfsSolution = drawPath mazeMatrix bfsPath
  savePngImage (outPath (mazeName ++ "_bfs")) (ImageRGB8 (getImageFromMatrix bfsSolution))
  print "========="

  print "== DFS =="

  start <- getCPUTime
  let dfsPath = dfs graph (head $ getNodes graph) (last $ getNodes graph)
  end  <- dfsPath `deepseq` getCPUTime
  let diff = fromIntegral (end - start) / (10^12)
  printf "DFS Time: %0.3f sec\n" (diff :: Double)
  
  let dfsSolution = drawPath mazeMatrix dfsPath
  savePngImage (outPath (mazeName ++ "_dfs")) (ImageRGB8 (getImageFromMatrix dfsSolution))
  print "========"

  print "== A* =="
  -- let aStarPath = aS ath (mazeName ++ "_AStar")) (ImageRGB8 (getImageFromMatrix dfsSolution))
  print "========"

  -- print ">>>"
  -- print $ graphToMatrix $ matrixToGraph [[0]]
  -- print $ graphToMatrix $ matrixToGraph [[]]
  -- print $ graphToMatrix (Graph [])
