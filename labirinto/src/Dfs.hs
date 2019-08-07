import Graph

dfs :: Eq a => Graph a -> Node a -> Node a -> [Node a]
dfs g u v  = getPath (dfs' g (u,u) v [] []) v
      where
        {-|
          Dfs' function returns a list of tuples, of type '(Node, Node)',
          which the first element is node visited and the second element is
          the predecessor of that node
          First argument is a graph, of type 'Graph'
          Second argument is a tuple containing a node and it's predecessor, of type '(Node, Node)'
          Third argument is the target node, of type 'Node'
          Fourth argument is a list of visited nodes with its predecessor, of type '(Node, Node)'
          Fifth argument is the queue of tuples of nodes and predecessors, of type '(Node, Node)'
        -}
        dfs' :: Eq a => Graph a -> (Node a, Node a) -> Node a -> [(Node a, Node a)] -> [(Node a, Node a)] -> [(Node a, Node a)]
        dfs' g (u, predU) v visited queue
          -- Checks if the node we're looking is the node we're looking for
          | u == v = (u,predU):visited

          -- Looks if the queue' is empty
          | queue' == [] = []

          -- Checks if we've already seen this node to avoid loops
          | u `elem` nodesVisited =  dfs' g (head queue) v visited queue'

          -- Calls dfs' passing next element in queue and adding the neighbors of u to front of queue
          | otherwise = dfs' g (head queue') v ((u,predU):visited) (tail queue')
              where
                nodesVisited  = [fst x | x <- visited]
                queue'        = neighbors ++ queue
                neighbors     = [(x, predX) | x <- (getAdjacent g u)]
                predX         = u

{-|
  GetPath function returns the path between nodes u and v 
  First argument is list of tuples, of type '(Node, Node)', which the first
  element is the node and the second element is it's predecessor
  Second argument is our node goal (a.k.a v in our dfs function)
-}
getPath :: Eq a => [(Node a, Node a)] -> Node a -> [Node a]
getPath [] _ = []
getPath (n:ns) pred
  | fst n == snd n = [fst n]
  | fst n == pred  = getPath ns (snd n)++[pred]
  | otherwise      = getPath ns pred
