module Parser (getMatrixFromImage, getImageFromMatrix) where
  
import Data.Either
import Control.Monad.ST

import Codec.Picture
import Codec.Picture.Types

-- Pixel resolution (TODO: remove)
resolution = 10

getPixels img = [[pixelAt img x y | x <- [0,resolution..width]] | y <- [0,resolution..height]]
  where
    width = imageWidth img - 1
    height = imageHeight img - 1

pixelToText (PixelRGB8 r g b)
  | r * g * b == 0 = '#'
  | otherwise = ' '

textToPixel c
  | c == '#'= PixelRGB8 0 0 0
  | otherwise = PixelRGB8 255 255 255

getMatrixFromImage :: Either String DynamicImage -> [String]
getMatrixFromImage img =
  case img of
    Left  _   -> [[]]
    Right img -> map (map pixelToText) $ getPixels $ convertRGB8 img


getImageFromMatrix :: [String] -> Image PixelRGB8
getImageFromMatrix matrix = runST $ do
  mimg <- newMutableImage imageWidth imageHeight
  let convert x y | x >= imageWidth  = convert 0 (y + 1)
                  | y >= imageHeight = unsafeFreezeImage mimg
                  | otherwise = do
                      writePixel mimg x y (textToPixel ((matrix !! y) !! x))
                      convert (x + 1) y
  convert 0 0
    where
      imageHeight = length matrix
      imageWidth = length $ head matrix
      