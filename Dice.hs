module Dice where

import Data.Bits
import Data.Char
import Data.List
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

type Name = String
type ImageName = String
type SimilarityScore = Float

bestMatches :: IO  [(Name, ImageName, SimilarityScore)]
bestMatches = do
    images <- lines <$> readFile "/Users/ccoffey/Downloads/school-images.txt"
    names <- lines <$> readFile "/Users/ccoffey/Downloads/school-names.txt"
    let indexedImgs =  makeQuickLookup $ cleanImgName <$> images
    pure $ bestMatch indexedImgs <$> names

bestMatch :: M.Map String [ImageName] -> Name -> (Name, ImageName, SimilarityScore)
bestMatch index name = fromMaybe def $ toResult . foldr takeBest ("", 0) <$> M.lookup (take 2 name) index
    where
    def = (name, "", 0.0)
    takeBest imgName curr@(cimg, currentBest) = let
        score = name .::. imgName
        in  if score > currentBest
            then (imgName, score)
            else curr
    toResult (imgName, score) = (name, imgName, score)

-- O(n * log n)
diceCoef :: String -> String -> SimilarityScore
diceCoef l r = matches lbs rbs / (fromIntegral $ (length l + length r) -2)
    where
    lbs = sort . optimize $ bigrams l
    rbs = sort . optimize $ bigrams r
    optimize = map (\(a,b) -> (ord a `shift` 16) .|. ord b)
    matches [] _ = 0
    matches _ [] = 0
    matches (x:xs) (y:ys)
        | x == y = 2 + matches xs ys
        | x < y = matches xs (y:ys)
        | otherwise = matches (x:xs) ys

infixr 7 .::.
(.::.) = diceCoef

bigrams :: [a] -> [(a, a)]
bigrams xs = xs `zip` tail xs

makeQuickLookup :: [ImageName] -> M.Map String [ImageName]
makeQuickLookup xs = foldr build M.empty xs
    where
    build s m = M.insertWith (<>) (take 2 s) [s] m

cleanImgName :: ImageName -> ImageName
cleanImgName = toSpaces . reverse . tail . dropWhile (/= '.') . reverse 
    where
    toSpaces [] = []
    toSpaces (c:rest) 
        | c == '_' = ' ': toSpaces rest
        | otherwise = c: toSpaces rest
