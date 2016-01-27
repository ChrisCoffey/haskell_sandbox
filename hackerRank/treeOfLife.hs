import Data.Bits                            (testBit)

data BTree a = ETree | Node (BTree a) a (BTree a) deriving (Show)
type CellState = [Bool] -> Bool
type CaRule a = CellState -> BTree a


root ETree = ETree
root (Node l v r) = v
left ETree = ETree
left (Node l v r) = l
right ETree = ETree
right (Node l v r) = r

parsePath :: String -> BTree a -> BTree a
parsePath str
    | str == "[]" = root
    | str == ('[':rest) = parsePath rest
    | str == ('<':rest) = (parsePath rest) . left 
    | str == ('>':rest) = (parsePath rest) . right
    | str == (']':[]) = root

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

parseTree :: String -> BTree a
parseTree str = f str where
    takeChunk s = 
    f ('(':rest) = 
    

main = do
    print "fail"
