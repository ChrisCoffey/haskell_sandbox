module Lib where

maximalSum :: [Integer] -> Integer
maximalSum ls = f ls 0 0 where
    f [] acc currentMax = max acc currentMax
    f (0:rest) acc currentMax = f rest 0 (max acc currentMax)
    f (h:rest) acc currentMax 
        | h < 0  && acc == 0 = f rest acc currentMax
        | otherwise = f rest (acc + h) currentMax


data Tree = Leaf Bool | Node [Tree] Bool
makeTree :: [Int] -> [Int]-> Tree
makeTree colors structure = let
    :
treeLength :: [[Int]]
