import Test.Hspec
import Test.QuickCheck

import Codec.Picture
import Codec.Picture.Types

import Control.Monad

import Graph
import qualified Parser

originalPath name = "resources/mazes/" ++ name ++ ".png"
outPath name = "test/mazes/" ++ name ++ "_out.png"

getMatrixes (img1, img2) = (m1, m2)
  where
    m1 = Parser.getMatrixFromImage img1
    m2 = Parser.getMatrixFromImage img2

-- instance Arbitrary (Node n) where
instance Arbitrary a => Arbitrary (Node a) where
  arbitrary = Node <$> arbitrary

instance Arbitrary a => Arbitrary (Edge a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Edge (x, y)

instance (Eq a, Arbitrary a) => Arbitrary (Graph a) where
  arbitrary = do
    nodeList <- listOf arbitrary

    edgeLists <- mapM (genEdgeList nodeList) nodeList

    return $ Graph (zip nodeList edgeLists)
      where
        genEdgeList nodeList n = do
          let nodeList' = filter (/= n) nodeList
          adjacents <- sublistOf =<< shuffle nodeList'

          let edges = map (\n' -> Edge (n, n')) adjacents
          return edges

main :: IO ()
main = hspec $ do
  describe "Parser" $ do
    it "Images read from same path are converted to the same matrix" $ do
      let mazeName = "MAZE01"
      img11 <- readImage $ originalPath mazeName
      img21 <- readImage $ originalPath mazeName
      let (m1, m2) = getMatrixes (img11, img21)
      m1 `shouldBe` m2

      let mazeName = "MAZE02"
      img12 <- readImage $ originalPath mazeName
      img22 <- readImage $ originalPath mazeName
      let (m1, m2) = getMatrixes (img12, img22)
      m1 `shouldBe` m2

      let mazeName = "MAZE03"
      img13 <- readImage $ originalPath mazeName
      img23 <- readImage $ originalPath mazeName
      let (m1, m2) = getMatrixes (img13, img23)
      m1 `shouldBe` m2

      let mazeName = "MAZE04"
      img14 <- readImage $ originalPath mazeName
      img24 <- readImage $ originalPath mazeName
      let (m1, m2) = getMatrixes (img14, img24)
      m1 `shouldBe` m2

      let mazeName = "MAZE05"
      img15 <- readImage $ originalPath mazeName
      img25 <- readImage $ originalPath mazeName
      let (m1, m2) = getMatrixes (img15, img25)
      m1 `shouldBe` m2

    it "Matrix parsed once is equals to matrix parsed twice" $ do
      let mazeName = "MAZE01"
      img1 <- readImage $ originalPath mazeName
      let matrixOnce = Parser.getMatrixFromImage img1

      -- save image from matrix parsed once
      savePngImage (outPath mazeName) (ImageRGB8 (Parser.getImageFromMatrix matrixOnce))

      img2 <- readImage $ outPath mazeName
      let matrixTwice = Parser.getMatrixFromImage img2
      matrixOnce `shouldBe` matrixTwice
    
  describe "Graph" $ do
    it "Test getFst" $ property testFst
    it "Test getSnd" $ property testSnd
    it "Test graph" $ property testGraph
    -- TODO: it "Test adjacent" $ property testAdjcencies


testFst :: Edge (Node Int, Node Int) -> Bool
testFst e@(Edge (a, b)) = getFst e == a

testSnd :: Edge (Node Int, Node Int) -> Bool
testSnd e@(Edge (a, b)) = getSnd e == b

-- Test if all the edges returned from solvers are present in the original graph

testGraph :: Graph [(Node Int, [Edge Int])] -> Bool
testGraph (Graph [(n, [])]) = True
testGraph (Graph [(n, l)]) = True
testGraph (Graph []) =  True