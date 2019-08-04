module Parser where
  
import Data.Char
import Data.Either
import Control.Monad.ST

import Codec.Picture
import Codec.Picture.Types

extractPixels img = [[pixelAt img x y | x <- [0..width]] | y <- [0..height]]
  where
    width = imageWidth img - 1
    height = imageHeight img - 1

pixelToDigit :: PixelRGB8 -> Int
pixelToDigit (PixelRGB8 r g b)
  | r * g * b == 0 = -1
  | otherwise = 0

digitToPixel :: Int -> Int -> PixelRGB8
digitToPixel p maxDigit
  | p == -1 = PixelRGB8 0 0 0 -- Black pixel for -1
  | p == 0 = PixelRGB8 255 255 255 -- White pixel for 0
  | otherwise = interpolatedPixel -- Intepolated pixel color for any other number
    where
      interpolatedPixel = PixelRGB8 (fromIntegral r) 0 (fromIntegral b)
      r = (p * 255) `quot` maxDigit
      b = (maxDigit - p) * 255 `quot` maxDigit

getMatrixFromImage :: Either String DynamicImage -> [[Int]]
getMatrixFromImage img =
  case img of
    Left  _   -> error "Can't load image."
    Right img -> map (map pixelToDigit) $ extractPixels $ convertRGB8 img


getImageFromMatrix :: [[Int]] -> Image PixelRGB8
getImageFromMatrix matrix = runST $ do
  mimg <- newMutableImage imageWidth imageHeight
  let convert x y | x >= imageWidth  = convert 0 (y + 1)
                  | y >= imageHeight = unsafeFreezeImage mimg
                  | otherwise = do
                      writePixel mimg x y (digitToPixel ((matrix !! y) !! x) maxDigit)
                      convert (x + 1) y
  convert 0 0
    where
      maxDigit = maximum $ concat matrix
      imageHeight = length matrix
      imageWidth = length $ head matrix
      