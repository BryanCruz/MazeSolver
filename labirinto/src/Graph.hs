module Graph where

newtype Node n = Node n deriving (Eq)
newtype Edge n = Edge (Node n, Node n) deriving (Show, Eq)
newtype Graph n = Graph [(Node n, [Edge n])]

instance Show a => Show (Node a) where
  show (Node x) = show x

instance Show a => Show (Graph a) where
  show (Graph []) = ""
  show (Graph (x:xs)) = show (fst x) ++ concatMap ((\x -> " -> " ++ show x) . getSnd) (snd x) ++ "\n" ++ show (Graph xs)

getFst :: Edge a -> Node a
getFst (Edge (a, b)) = a

getSnd :: Edge a -> Node a
getSnd (Edge (a, b)) = b

getAdjacent :: Eq a => Graph a -> Node a -> [Node a]
getAdjacent (Graph vs) u = concat [map getSnd es | (n, es) <- vs, n == u]
