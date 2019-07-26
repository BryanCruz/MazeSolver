module Main where

import Data.Either
import Codec.Picture

testSave = saveBmpImage "src/testBmp.bmp"

dynPixelSave img l = dynamicPixelMap (\x y -> l ++ [(x,y)])

main :: IO ()
main = do
  img <- convertRGB8 $ readImage "src/MAZE01.png"
  case img of
    Left s -> print $ "ERRO: " ++ s
    Right img -> testSave img