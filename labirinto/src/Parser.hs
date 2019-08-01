module Parser (getMatrixFromImage) where
  
import Data.Either
import Codec.Picture

-- Pixel resolution (TODO: remove)
resolution = 10

getPixels img = [[pixelAt img x y | x <- [0,resolution..width]] | y <- [0,resolution..height]]
  where
    width = imageWidth img - 1
    height = imageHeight img - 1

pixelToText (PixelRGB8 r g b)
  | r * g * b == 0 = '#'
  | otherwise = ' '

getMatrixFromImage :: Either String DynamicImage -> [String]
getMatrixFromImage img =
  case img of
    Left  _   -> [[]]
    Right img -> map (map pixelToText) $ getPixels $ convertRGB8 img

