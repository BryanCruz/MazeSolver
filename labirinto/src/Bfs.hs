module Bfs where

import Graph

import Debug.Trace

{-|
  Bfs function returns the shortest path between two nodes in a graph
  First argument is a graph of type 'Graph'
  Second argument is the start node of type 'Node'
  Third argument is the target node of type 'Node'
  -}
bfs :: (Show a, Eq a) => Graph a -> Node a -> Node a -> [Node a]
bfs g u v = buildPath (bfs' g (u,u) v [] []) v
      where
        {-|
          Bfs' function returns a list of tuples, of type '(Node, Node)',
          which the first element is node visited and the second element is
          the predecessor of that node
          First argument is a graph, of type 'Graph'
          Second argument is a tuple containing a node and it's predecessor, of type '(Node, Node)'
          Third argument is the target node, of type 'Node'
          Fourth argument is a list of visited nodes with its predecessor, of type '(Node, Node)'
          Fifth argument is the queue of tuples of nodes and predecessors, of type '(Node, Node)'
        -}
        bfs' :: (Show a, Eq a) => Graph a -> (Node a, Node a) -> Node a -> [(Node a, Node a)] -> [(Node a, Node a)] -> [(Node a, Node a)]
        bfs' g (u, predU) v visited queue
          -- Checks if the node we're looking is the node we're looking for
          | u == v = (u,predU):visited
          
          -- Looks if the queue' is empty
          | queue' == [] = []
          
          -- Checks if we've already seen this node to avoid loops
          | u `elem` nodesVisited =  bfs' g (head queue) v visited queue'
          
          -- Calls bfs' passing next element in queue and adding the neighbors of u to queue
          | otherwise = trace (show visited) $ bfs' g (head queue') v ((u,predU):visited) (tail queue')
              where 
                nodesVisited  = [fst x | x <- visited]
                queue'        = queue ++ neighbors
                neighbors     = [(x, predX) | x <- (getAdjacent g u), not (x `elem` nodesVisited)]
                predX         = u

        {-|
          BuildPath function returns the path between nodes u and v 
          First argument is list of tuples, of type '(Node, Node)', which the first
          element is the node and the second element is it's predecessor
          Second argument is our node goal (a.k.a v in our bfs function)
        -}
        buildPath :: Eq a => [(Node a, Node a)] -> Node a -> [Node a]
        buildPath [] _ = []
        buildPath (n:ns) pred
          | fst n == snd n = [fst n]
          | fst n == pred  = buildPath ns (snd n)++[pred]
          | otherwise      = buildPath ns pred

