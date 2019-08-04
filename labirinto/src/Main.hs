module Main where

import Parser
import Codec.Picture
import Codec.Picture.Types

mazeName :: String
mazeName = "MAZE02"

originalPath :: String -> FilePath
originalPath name = "src/mazes/" ++ name ++ ".png"

outPath :: String -> FilePath
outPath name = "src/mazes/" ++ name ++ "_out.png"

toChar :: [[Int]] -> String
toChar xss = concatMap (replaceLast . concat) css
  where
    css = map (map (\x -> show x ++ " ")) xss
    replaceLast ls = init ls ++ "\n"

toNumber :: [[String]] -> [[Int]]
toNumber css = map (map read) css

main :: IO ()
main = do
  -- Read image is an IO action
  mazeImage <- readImage $ originalPath mazeName

  -- Parse image to a Maze Matrix
  let mazeMatrix = getMatrixFromImage mazeImage

  -- For debugging, maze matrix may be printed
  -- print $ toNumber $ map words $ lines mazeMatrix

  -- Save Image is an IO action
  savePngImage (outPath mazeName) (ImageRGB8 (getImageFromMatrix mazeMatrix))
  