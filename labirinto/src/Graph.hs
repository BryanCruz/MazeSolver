module Graph where

data Node n = Node n deriving (Show, Eq)

data Edge n = Edge (Node n) (Node n) deriving (Show, Eq)

data Graph n = Graph [Node n] [Edge n] deriving (Show)

getFst :: Edge a -> Node a
getFst (Edge a b) = a

getSnd :: Edge a -> Node a
getSnd (Edge a b) = b
