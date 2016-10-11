module AirplaneSudoku where

import Data.List (any, (\\))
import qualified Data.Map.Lazy as M


data Cell = Cell {
    x:: Int,
    y:: Int,
    constraints:: [Int],
    val:: Maybe Int
} deriving (Show)

instance Eq Cell where
    equals

row :: Cell -> [Cell] -> [Cell]
row (Cell rowNum _ _ _)= filter ((== rowNum) . x) 

column :: Cell -> [Cell] -> [Cell]
column (Cell _ colNum _ _) = filter ((== colNum) . y) 

sector :: Cell -> [Cell] -> [Cell]
sector c = filter ((== sectorNum c) . sectorNum)
    where sectorNum (Cell x y _ _) = (x `mod` 3, y `mod` 3)

rowConstraints :: Cell -> [Cell] -> [Int]
rowConstraints c = fmap (\c) . row c 
