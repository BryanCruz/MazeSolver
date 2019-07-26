module Parser (getMazeMatrix) where
  
import Data.Either
import Codec.Picture

getImagePath name = "src/mazes/" ++ name ++ ".png"

-- Pixel resolution (TODO: remove)
resolution = 10

getPixels img = [[pixelAt img x y | x <- [0,resolution..width]] | y <- [0,resolution..height]]
  where
    width = (imageWidth img) - 1
    height = (imageHeight img) - 1

pixelToText (PixelRGB8 r g b)
  | r * g * b == 0 = '#'
  | otherwise = ' '

getMazeMatrix :: [Char] -> IO [[Char]]
getMazeMatrix name = do
  let path = getImagePath name
  img <- readImage path

  case img of
    Left  _   -> return []
    Right img -> return $ map (map pixelToText) $ getPixels $ convertRGB8 img
