module Main where

import Parser
import Codec.Picture
import Codec.Picture.Types
import System.Environment

originalPath :: String -> FilePath
originalPath name = "resources/mazes/" ++ name ++ ".png"

outPath :: String -> FilePath
outPath name = "output/mazes/" ++ name ++ "_out.png"

main :: IO ()
main = do
  -- Get maze name from command line
  args <- getArgs

  let mazeName = if not $ null args then head args else "MAZE01"

  -- Read image is an IO action
  mazeImage <- readImage $ originalPath mazeName

  -- Parse image to a Maze Matrix
  let mazeMatrix = getMatrixFromImage mazeImage

  -- Save Image is an IO action
  savePngImage (outPath mazeName) (ImageRGB8 (getImageFromMatrix mazeMatrix))
