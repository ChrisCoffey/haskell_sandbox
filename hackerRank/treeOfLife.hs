import Control.Applicative
import Control.Monad
import Data.Bits                            (testBit)
import Data.Char
import Data.Functor


newtype Parser a = Parser {parse :: String -> [(a,String)]}

data BTree a = Leaf a | Node (BTree a) a (BTree a) deriving (Show)
type CellState = [Bool] -> Bool
type CaRule a = CellState -> BTree a

root:: BTree a -> BTree a
root l@(Leaf v)= l
root (Node l v r) = Leaf v

left:: BTree a -> BTree a
left (Node l v r) = l

right :: BTree a -> BTree a
right (Node l v r) = r

parsePath :: String -> BTree a -> BTree a
parsePath str@(c:rest)
    | str == "[]"       = root
    | c == '['          = parsePath rest
    | c == '<'          = (parsePath rest) . left 
    | c == '>'          = (parsePath rest) . right
    | str == (']':[])    = root

parseRule :: Int -> CellState
parseRule n = let
    bits = [testBit n i | i <- [15,14..0]]
    stateMap = s where
        s [True, True, True, True]      = bits !! 15
        s [True, True, True, False]     = bits !! 14 
        s [True, True, False, True]     = bits !! 13 
        s [True, True, False, False]    = bits !! 12
        s [True, False, True, True]     = bits !! 11
        s [True, False, True, False]    = bits !! 10 
        s [True, False, False, True]    = bits !! 9
        s [True, False, False, False]   = bits !! 8
        s [False, True, True, True]     = bits !! 7
        s [False, True, True, False]    = bits !! 6
        s [False, True, False, True]    = bits !! 5
        s [False, True, False, False]   = bits !! 4
        s [False, False, True, True]    = bits !! 3
        s [False, False, True, False]   = bits !! 2
        s [False, False, False, True]   = bits !! 1
        s [False, False, False, False]  = bits !! 0
   in stateMap 

parseTree :: String -> BTree Bool
parseTree str = parse str where
    midpoint 1 (')':xs)     = xs
    midpoint n ('(':xs)     = midpoint (n+1) xs 
    midpoint n (')':xs)     = midpoint (n-1) xs
    midpoint n (x:xs)       = midpoint n xs

    takeSubtree ('(':xs) 0  = takeSubtree xs 1
    takeSubtree (')':xs) 1  = []
    takeSubtree ('(':xs) n  = '(':(takeSubtree xs (n + 1))
    takeSubtree (')':xs) n  = ')':(takeSubtree xs (n - 1))
    takeSubtree (x:xs) n    = x:(takeSubtree xs n)
    
    parse :: String -> BTree Bool
    parse (' ':xs) = parse xs
    parse ('X':xs) = Leaf True
    parse ('.':xs) = Leaf False
    parse(')':xs) = 
    parse seg@('(':cs) =  let 
        l = (parse $ takeSubtree seg 0)
        (Leaf v) = parse $ midpoint 0 seg
        r = parse $ takeSubtree (drop 3 . midpoint 0 $ seg) 0
        in Node l v r

main = do
    ls <- lines <$> getContents
    let 
        rule = ls !! 0
        tree = ls !! 1
        cases = drop 3 ls
    print tree
