module Dfs where

import Graph

dfs :: Eq a => Graph a -> Node a -> Node a -> [Node a]
dfs g st end = dfs' g st end st
  where
    dfs' g st end par | st /= end = if null recursion then [] else st : concat recursion
                      | otherwise = [st]
      where 
        recursion = dropWhile null $ map (\v -> dfs' g v end st) $ filter (/= par) $ getAdjacent g st
