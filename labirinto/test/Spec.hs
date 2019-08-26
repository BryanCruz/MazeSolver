import Test.Hspec
import Test.QuickCheck

import Codec.Picture
import Codec.Picture.Types

import Data.List
import Debug.Trace
import Control.Monad
import Control.Applicative

import Bfs
import Dfs
import Graph
import qualified Parser
import qualified GraphConverter as GC

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
    nodeList <- nub <$> listOf arbitrary
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
    -- As images do not derive Show, we can't compare'em with `should be`, which is kind of a flaw in this tests
    -- As an attempt to workaround this issue, we are testing matrixes parsed from images
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
  
  describe "DrawPath" $
    it "Matrix with path should have the same shape as original matrix" $ do
      (l10, l20) <- getLengthsOfPath "MAZE00"
      l10 `shouldBe` l20

      (l11, l21) <- getLengthsOfPath "MAZE01"
      l11 `shouldBe` l21

      (l12, l22) <- getLengthsOfPath "MAZE02"
      l12 `shouldBe` l22

      (l13, l23) <- getLengthsOfPath "MAZE03"
      l13 `shouldBe` l23

      (l14, l24) <- getLengthsOfPath "MAZE04"
      l14 `shouldBe` l24

      (l15, l25) <- getLengthsOfPath "MAZE05"
      l15 `shouldBe` l25
  
  describe "Graph" $ do
    it "Test getFst" $ property testFst
    it "Test getSnd" $ property testSnd
    it "Test Nodes Present" $ property $ testNodesPresent dfs
    it "Test Edges Present" $ property $ testEdgesPresent dfs

getLengthsOfPath mazeName = do
  img1 <- readImage $ originalPath mazeName
  let matrix = Parser.getMatrixFromImage img1

  let g = GC.matrixToGraph matrix
  let nodes = getNodes g
  let (p, _) = dfs g (head nodes) (last nodes)
  let drawn = GC.drawColorMap matrix p
  return ((length matrix, length $ head matrix), (length drawn, length $ head drawn))

testFst :: Edge (Node Int, Node Int) -> Bool
testFst e@(Edge (a, b)) = getFst e == a

testSnd :: Edge (Node Int, Node Int) -> Bool
testSnd e@(Edge (a, b)) = getSnd e == b

-- Test if all nodes returned from solvers are present in the original graph
testNodesPresent :: (Graph Int -> Node Int -> Node Int -> ([Node Int], [Node Int])) -> Graph Int -> Bool
testNodesPresent alg g = all (`elem` graphNodes) pathNodes
  where
    graphNodes = getNodes g
    (pathNodes, _)  = if not $ null graphNodes
                      then alg g (head graphNodes) (last graphNodes)
                      else ([], [])

-- Test if all edges returned from solvers are present in the original graph
testEdgesPresent :: (Graph Int -> Node Int -> Node Int -> ([Node Int], [Node Int])) -> Graph Int -> Bool
testEdgesPresent alg g = all (`elem` graphEdges) pathEdges
  where
    graphEdges = getEdges g
    graphNodes = getNodes g

    (pathNodes, _) = alg g (head graphNodes) (last graphNodes)
    pathEdges = if not $ null graphEdges
                then nodesToEdges pathNodes
                else []

-- Parse list of nodes to list of edges that connects'em
nodesToEdges :: [Node a] -> [Edge a]
nodesToEdges ns = if not $ null e then init e else []
  where
    e = zipWith (curry Edge) ns (tail ns)