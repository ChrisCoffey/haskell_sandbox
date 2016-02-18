import Control.Monad
import Data.Functor
import qualified Data.Map           as M

newtype Var = Var String 
    deriving Show
newtype N   = N Integer 
    deriving Show
data Oα     = Plus | Minus | Mult | Div
    deriving (Show)
data Oβ     = And | Or
    deriving (Show)
data Oρ     = Gt | Lt
    deriving (Show)
data AExp   = ExpAV Var | ExpAN N | ExpAO Oα AExp Oα | ExpAP AExp 
    deriving (Show)
data BExp   = T | F | BExpB BExp Oβ BExp | BExpR AExp Oρ AExp | BExpP BExp 
    deriving (Show)
data Stmt   = ASSIGN Var AExp | SEQ Stmt Stmt | IF BExp Stmt Stmt | WHILE BExp Stmt
    deriving (Show)

newtype PState = PState (M.Map String N)
newtype Prog   = Prog (M.Map Int Stmt) -- not thrilled with representing a program as a simple sequence of statements

--need to implement the interpreter for the language above
lookupVar (Var s) (PState m) = M.lookup s m
extractN (N i) = i
(N l) +: (N r) = N (l + r)
(N l) -: (N r) = N (l - r)
(N l) *: (N r) = N (l * r)
(N l) /: (N r) = N (l `div` r)
l  &: r = f l r where
    f T T = T
    f T F = F
    f F T = F
    f F F = F
l |: r = f l r where
    f T _ = T
    f _ T = T
    f a b = F
(N l) <: (N r)
    | l < r = T 
    | otherwise = F 
(N l) >: (N r)
    | l > r = T 
    | otherwise =  F 


--need to implement a recursive descent parser

main = do
    ls <- getContents
    print "not implemented"
