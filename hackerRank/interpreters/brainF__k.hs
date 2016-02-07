import qualified Data.Set                   as S
import qualified Data.List                  as L

data Zipper a = Zip [a] [a] deriving (Eq, Show)

fromList :: [a] -> Zipper a
fromList xs = Zip [] xs

cursor :: Zipper a -> a
cursor (Zip l (r:rest)) = r

left :: Zipper a -> Zipper a
left z@(Zip l (r:[])) = z
left z@(Zip l (r:rs)) = Zip (l ++[r]) rs

right :: Zipper a -> Zipper a
right z@(Zip [] rs) = z
right z@(Zip l r) = Zip (take ((length l) -1) l) ((last l):r)

replace :: a -> Zipper a -> Zipper a
replace a (Zip l (r:rs)) = Zip l (a:rs)

--Evaluating the interpreter is a zipper over the instructions (program)
tokens = S.fromList [',', '+', '-', '<', '>', '.', '[', ']']
type Memory = Zipper Int
type Program = Zipper Char
type Input = String
type Output = String

data BfState = BfState Memory Program Input Output Ops

incPtr :: Memory -> Memory
incPtr mem = right mem

decPtr :: Memory -> Memory
decPtr mem = left mem

incAddr :: Memory -> Memory
incAddr mem = f (cursor mem) where
    f 255 = replace 0 mem
    f x = replace (x + 1) mem
    
decAddr :: Memory -> Memory
decAddr mem = f (cursor mem) where
    f 0 = replace 255 mem
    f x = replace (x - 1) mem

interpret :: BfState  -> BfState
interpret (BfState m p i o 100000) = BfState m p i "PROCESS TIME OUT. KILLED!!!" -1
interpret (BfState mem prog i o ops) = run '*'where
    run searchChar 
        | searchChar ']' = if (cursor prog) == ']'
                           then interpret (BfState mem (right prog i o (ops +1))) 
        | searchChar '[' = 
        | otherwise      =
    


main = do
    print "not implemented"
