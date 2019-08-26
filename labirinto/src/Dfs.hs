module Dfs where

  import Graph

  dfs :: Eq a => Graph a -> Node a -> Node a -> ([Node a], [Node a])
  dfs g u v = (resultPath, visitedNodes)
        where
          result = (dfs' g (u,u) v [] [])
          visitedNodes = getVisited result
          resultPath = buildPath result v
          {-|
            Dfs' function returns a list of tuples, of type '(Node, Node)',
            which the first element is node visited and the second element is
            the predecessor of that node
            First argument is a graph, of type 'Graph'
            Second argument is a tuple containing a node and it's predecessor, of type '(Node, Node)'
            Third argument is the target node, of type 'Node'
            Fourth argument is a list of visited nodes with its predecessor, of type '(Node, Node)'
            Fifth argument is the stack of tuples of nodes and predecessors, of type '(Node, Node)'
          -}
          dfs' :: Eq a => Graph a -> (Node a, Node a) -> Node a -> [(Node a, Node a)] -> [(Node a, Node a)] -> [(Node a, Node a)]
          dfs' g (u, predU) v visited stack
            -- Checks if the node we're looking is the node we're looking for
            | u == v = (u,predU):visited
  
            -- Looks if the stack' is empty
            | stack' == [] = []
  
            -- Checks if we've already seen this node to avoid loops
            | u `elem` nodesVisited =  dfs' g (head stack) v visited (tail stack)
  
            -- Calls dfs' passing next element in stack and adding the neighbors of u to front of stack
            | otherwise = dfs' g (head stack') v ((u,predU):visited) (tail stack')
                where
                  nodesVisited  = [fst x | x <- visited]
                  stack'        = neighbors ++ stack
                  neighbors     = [(x, predX) | x <- (getAdjacent g u), x `notElem` nodesVisited]
                  predX         = u
  
          {-|
            BuildPath function returns the path between nodes u and v 
            First argument is list of tuples, of type '(Node, Node)', which the first
            element is the node and the second element is it's predecessor
            Second argument is our node goal (a.k.a v in our dfs function)
          -}
          buildPath :: Eq a => [(Node a, Node a)] -> Node a -> [Node a]
          buildPath [] _ = []
          buildPath (n:ns) pred
            | fst n == snd n = [fst n]
            | fst n == pred  = buildPath ns (snd n)++[pred]
            | otherwise      = buildPath ns pred

          getVisited ns = reverse [fst n | n <- ns]
