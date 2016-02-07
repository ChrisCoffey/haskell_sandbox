import qualified Data.Set                   as S
import qualified Data.List                  as L
import Data.Functor
import Control.Monad
import Data.Char

data Zipper a = Zip [a] [a] deriving (Eq, Show)

fromList :: [a] -> Zipper a
fromList xs = Zip [] xs

cursor :: Zipper a -> a
cursor (Zip l (r:rest)) = r

right:: Zipper a -> Zipper a
right z@(Zip l []) = z
right z@(Zip l (r:rs)) = Zip (l ++[r]) rs

left :: Zipper a -> Zipper a
left z@(Zip [] rs) = z
left z@(Zip l r) = Zip (take ((length l) -1) l) ((last l):r)

replace :: a -> Zipper a -> Zipper a
replace a (Zip l (r:rs)) = Zip l (a:rs)

--Evaluating the interpreter is a zipper over the instructions (program)
tokens = S.fromList [',', '+', '-', '<', '>', '.', '[', ']']
type Memory = Zipper Int
type Program = Zipper Char
type Input = String
type Output = String

data BfState = BfState Memory Program Input Output Int

incAddr :: Memory -> Memory
incAddr mem = f (cursor mem) where
    f 255 = replace 0 mem
    f x = replace (x + 1) mem
    
decAddr :: Memory -> Memory
decAddr mem = f (cursor mem) where
    f 0 = replace 255 mem
    f x = replace (x - 1) mem

interpret :: BfState  -> BfState
interpret (BfState m p i o 100000) = BfState m p i "PROCESS TIME OUT. KILLED!!!" 10000 -- out of time
interpret s@(BfState _ (Zip l [ ]) _ _ _) = s -- program termination
interpret s@(BfState mem prog i o ops) = run '*' s where
    runCmd c 
        | c == ',' = let
            (h:rest) = i
            b = ord h
            in BfState (replace b mem) (right prog) rest o (ops + 1)
        | c == '.' = let
            b = chr (cursor mem)
            in BfState mem (right prog) i (o++[b]) (ops + 1)
        | c == '+' = BfState (incAddr mem) (right prog) i o (ops + 1)
        | c == '-' = BfState (decAddr mem) (right prog) i o (ops + 1)
        | c == '>' = BfState (right mem) (right prog) i o (ops + 1)
        | c == '<' = BfState (right mem) (right prog) i o (ops + 1)
        | c == '[' = if (cursor mem) == 0
                     then run ']' (BfState mem (right prog) i o (ops +1))
                     else BfState mem (right prog) i o (ops + 1)
        | c == ']' = if (cursor mem) /= 0
                     then run '[' (BfState mem (left prog) i o (ops +1))
                     else BfState mem (left prog) i o (ops + 1)
    run searchChar state
        | searchChar == ']' = if (cursor prog) == ']'
                           then interpret (BfState mem (right prog) i o (ops +1))
                           else run searchChar  (BfState mem (right prog) i o (ops +1))
        | searchChar == '[' = if (cursor prog) == '['
                           then interpret (BfState mem (right prog) i o (ops +1))
                           else run searchChar  (BfState mem (left prog) i o (ops +1))
        | otherwise      =  interpret $ runCmd (cursor prog)

stripComments ln = 
    foldr (\x acc -> if S.member x tokens then x:acc else acc) "" ln

main = do
    cs <- lines <$> getContents
    let (h:t) = cs
        (a:b:[]) = map (\x -> read x :: Int) . words $ h
        input = take a . head $ t
        prog = L.concatMap stripComments . tail $ t
        initialState = BfState (fromList (repeat 0)) (fromList prog) input "" 0
        (BfState _ _ _ o _) = interpret initialState
    putStr o
    
