module Graph where

data Node n = Node n deriving (Show, Eq)

data Edge n = Edge (Node n) (Node n) deriving (Show, Eq)

data Graph n = Graph [Node n] [Edge n] deriving (Show)
