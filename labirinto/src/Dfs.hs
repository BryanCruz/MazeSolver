module Dfs where

import qualified Data.Map.Strict as Map

import Graph

dfs :: (Ord a, Eq a) => Graph a -> Node a -> Node a -> [Node a]
dfs g st end = dfs' g' st end st
  where
    g' = foldr (\n m -> insert n True m) empty g

    dfs' g st end par | st /= end = if null recursion g' then [] else st : concat recursion
                      | otherwise = [st]

    recursion g' = dropWhile null $ map (\v -> dfs' g v end st) $ filter (\n -> g' ! n) $ getAdjacent g st
        