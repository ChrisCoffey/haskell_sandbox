module AirplaneSudoku where

import Data.List (any)

type Row = [Int]
type Box = [Row]
newtype Puzzle = Puzzle [Row]
    deriving (Show, Eq)

solve :: Puzzle -> Puzzle
solve (Puzzle p) = undefined
--Need to branch the search for each possible valid value at each cell
--If all values fail, terminate that branch
--All branches should fail except for one, which is the solution

isSolution Puzzle -> Bool
isSolution (Puzzle p) = all solvedRow p
    where solvedRow = notElem (-1)

rotateR :: [Row]-> [Row]
rotateR = f 
    where 
    f [] = []
    f ([]:xs) = []
    f xs = (head <$> xs):(f (tail <$> xs))


canPlace :: Int -> Int -> Int -> Puzzle -> Bool
canPlace n x y (Puzzle p) = checkRow p x && checkRow (rotateR p) y && checkBox
    where 
    checkRow z = notElem n . (z !!)
    checkBox = foldl (\b r -> f r) False box
        where 
        f = notElem n
        xc = (take 3 . drop (3 * (x `mod` 3))) <$> p-- Broke down and went with 9 x 9. Easy to generalize
        box = take 3 . drop (3 * (y `mod` 3)) $ p
        
        

