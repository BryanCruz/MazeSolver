module Parser (getMatrixFromImage) where
  
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
  | otherwise = PixelRGB8 100 100 100

getMatrixFromImage :: Either String DynamicImage -> [String]
getMatrixFromImage img =
  case img of
    Left  _   -> [[]]
    Right img -> map (map pixelToText) $ getPixels $ convertRGB8 img

-- getImageFromMatrix :: [String] -> DynamicImage
-- getImageFromMatrix = map (map textToPixel)