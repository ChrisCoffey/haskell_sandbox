module Lib where

arraySum :: [Int] -> Int
arraySum as = f as 0 where
    f [] n = n
    f (h:rest) n = f rest (h + n)

sherlockBeast
