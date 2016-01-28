import Data.Bits                            (testBit)
import Data.Text                            (splitOn, pack, strip, unpack, singleton)

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
parseTree str = f str where
    takeBlocks n (c:rest)
        | c == '(' && n == 0 = takeBlocks (n + 1) rest 
        | c == '(' && n == 1 = '|':c:(takeBlocks (n+1) rest )
        | c == '(' && n > 0 =  c:(takeBlocks (n + 1) rest )
        | c == ')' && n == 2 = c:'|':(takeBlocks (n-1) rest )
        | c == ')' && n > 1 = c:(takeBlocks (n - 1) rest )
        | c == ')' && n == 1 = []
        | otherwise = c:(takeBlocks n rest )
    f "." = Leaf False
    f "X" = Leaf True
    f x = let
     grps = map (unpack . strip) . splitOn (singleton '|' ). pack . takeBlocks 0 $ str
     --need to properly unpack here too
     val = if v == "X" then True else False
     in Node (f l) val  (f r)
    

main = do
    print "fail"
