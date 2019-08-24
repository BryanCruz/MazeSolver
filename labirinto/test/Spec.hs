import Test.Hspec
import Test.QuickCheck

import Codec.Picture
import Codec.Picture.Types

import Parser

originalPath name = "resources/mazes/" ++ name ++ ".png"
outPath name = "test/mazes/" ++ name ++ "_out.png"

getMatrixes (img1, img2) = (m1, m2)
  where
    m1 = getMatrixFromImage img1
    m2 = getMatrixFromImage img2

main :: IO ()
main = hspec $
  describe "Parser" $ do
    it "Images read from same path are converted to the same matrix" $ do
      let mazeName = "MAZE01"
      img11 <- readImage $ originalPath mazeName
      img21 <- readImage $ originalPath mazeName
      let (m1, m2) = getMatrixes (img11, img21)
      getMatrixFromImage img11 `shouldBe` getMatrixFromImage img21

      let mazeName = "MAZE02"
      img12 <- readImage $ originalPath mazeName
      img22 <- readImage $ originalPath mazeName
      let (m1, m2) = getMatrixes (img12, img22)
      getMatrixFromImage img12 `shouldBe` getMatrixFromImage img22

      let mazeName = "MAZE03"
      img13 <- readImage $ originalPath mazeName
      img23 <- readImage $ originalPath mazeName
      let (m1, m2) = getMatrixes (img13, img23)
      getMatrixFromImage img13 `shouldBe` getMatrixFromImage img23

      let mazeName = "MAZE04"
      img14 <- readImage $ originalPath mazeName
      img24 <- readImage $ originalPath mazeName
      let (m1, m2) = getMatrixes (img14, img24)
      getMatrixFromImage img14 `shouldBe` getMatrixFromImage img24

      let mazeName = "MAZE05"
      img15 <- readImage $ originalPath mazeName
      img25 <- readImage $ originalPath mazeName
      let (m1, m2) = getMatrixes (img15, img25)
      getMatrixFromImage img15 `shouldBe` getMatrixFromImage img25

    it "Matrix parsed once is equals to matrix parsed twice" $ do
      let mazeName = "MAZE01"
      img1 <- readImage $ originalPath mazeName
      let matrixOnce = getMatrixFromImage img1

      -- save image from matrix parsed once
      savePngImage (outPath mazeName) (ImageRGB8 (getImageFromMatrix matrixOnce))

      img2 <- readImage $ outPath mazeName
      let matrixTwice = getMatrixFromImage img2

      matrixOnce `shouldBe` matrixTwice
      