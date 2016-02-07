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

rightWhile :: (a -> Bool) ->  Zipper a -> Maybe (Int, Zipper a)
rightWhile p zp = f 0 zp
    where
        f dist zp'
            | p (cursor zp') = f (dist + 1) (right zp')
            | rEmpty zp'     = Nothing
            | otherwise     = Just (dist, zp')

leftWhile :: (a -> Bool) -> Zipper a -> Maybe (Int, Zipper a)
leftWhile p zp = f 0 zp
    where 
        f dist zp'
            | p (cursor zp') = f (dist + 1) (left zp')
            | rEmpty zp'     = Nothing
            | otherwise     = Just (dist, zp')

rEmpty :: Zipper a -> Bool
rEmpty (Zip l []) = True
rEmpty z          = False

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

inc x = x + 1

interpret :: BfState  -> BfState
interpret s = run s 
    where
        runCmd :: Char -> BfState -> BfState
        runCmd c (BfState mem prog i o ops)
            | c == ','                  = let
                                            (h:rest) = i
                                            b = ord h
                                            in BfState (replace b mem) (right prog) rest o (inc ops)
            | c == '.'                  = let
                                            b = chr (cursor mem)
                                            in BfState mem (right prog) i (o++[b])  (inc ops)
            | c == '+'                  = BfState (incAddr mem) (right prog) i o (inc ops)
            | c == '-'                  = BfState (decAddr mem) (right prog) i o (inc ops)
            | c == '>'                  = BfState (right mem) (right prog) i o (inc ops)
            | c == '<'                  = BfState (left mem) (right prog) i o (inc ops)
            | c == '[' 
              && (cursor mem) == 0      = case rightWhile ( /= ']') prog of 
                                                Nothing      -> s
                                                Just (d,p)   -> (BfState mem p i o (d + ops))

            | c == '['                  = BfState mem (right prog) i o (inc ops)
            | c == ']'
              && (cursor mem) /= 0      = case leftWhile ( /= '[') prog of 
                                                Nothing      -> s
                                                Just (d,p)   -> (BfState mem p i o (d + ops))
            | c == ']'                  = BfState mem (right prog) i o (inc ops)

        run bf@(BfState mem' prog' i' o' ops')
            | ops' >= 100000            = BfState mem' prog' i' ((if (length o') >0 then o' ++ ['\n'] else o') ++ "PROCESS TIME OUT. KILLED!!!") 10000 -- out of time
            | rEmpty prog'              = bf
            | otherwise                 =  run (runCmd (cursor prog') bf)

cleanLine ln =
    foldr (\x acc -> if S.member x tokens then x:acc else acc) "" ln

main = do
    cs <- lines <$> getContents
    let (h:t) = cs
        (a:b:[]) = map (\x -> read x :: Int) . words $ h
        input = take a . head $ t
        prog = L.concatMap cleanLine . tail $ t
        initialState = BfState (fromList (repeat 0)) (fromList prog) input "" 0
        (BfState _ _ _ o _) = interpret initialState
    putStr o
