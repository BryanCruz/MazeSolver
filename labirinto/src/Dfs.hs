module Dfs where

import qualified Data.Map as Map

import Graph

dfs :: (Ord a, Eq a, Show a) => Graph a -> Node a -> Node a -> [Node a]
dfs g st end = dfs' gMap st end
  where
    gMap = foldr (\(Node n) m -> Map.insert n True m) Map.empty (getNodes g)
    dfs' gMap st end | st /= end = if null recursion'  then [] else st : (head recursion')
                     | otherwise = [st]
      where
        recursion' = recursion gMap

        recursion gMap = filter (\x -> not $ null x) $ map (\v -> dfs' gMap' v end) notVisitedYet
          where
            notVisitedYet = filter (\(Node n) -> gMap Map.! n) $ getAdjacent g st
            gMap' = foldr (\(Node n) m -> Map.insert n False m) gMap notVisitedYet
