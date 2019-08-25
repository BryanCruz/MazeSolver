module Main where

import Codec.Picture
import Codec.Picture.Types
import System.Environment

import Bfs
import Dfs
import Graph
import Parser
import GraphConverter

originalPath :: String -> FilePath
originalPath name = "resources/mazes/" ++ name ++ ".png"

outPath :: String -> FilePath
outPath name = "output/mazes/" ++ name ++ "_out.png"

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

  -- Parse image to a Maze Matrix
  let mazeMatrix = getMatrixFromImage mazeImage
  let graph = matrixToGraph mazeMatrix

  let path = dfs graph (head $ getNodes graph) (last $ getNodes graph)

  let matrixSolved = drawPath mazeMatrix path

  savePngImage (outPath mazeName) (ImageRGB8 (getImageFromMatrix matrixSolved))

  -- -- Save Image is an IO action
  -- savePngImage (outPath mazeName) (ImageRGB8 (getImageFromMatrix mazeMatrix))
