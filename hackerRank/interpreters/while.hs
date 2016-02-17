import Control.Monad
import Data.Functor
import qualified Data.Map           as M

newtype Var = Var String
newtype N   = N Integer
data Oα     = Plus | Minus | Mult | Div
data Oβ     = And | Or
data Oρ     = Gt | Lt
data AExp   = ExpAV Var | ExpAN N | ExpAO Oα AExp Oα | ExpAP AExp
data BExp   = T | F | BExpB BExp Oβ BExp | BExpR AExp Oρ AExp | BExpP BExp 
data Stmt   = ASSIGN Var AExp | SEQ Stmt Stmt | IF BExp Stmt Stmt | WHILE BExp Stmt

newtype PState = PState (M.Map String N)
newtype Prog   = Prog (M.Map Int Stmt) -- not thrilled with representing a program as a simple sequence of statements

--need to implement the interpreter for the language above
lookupVar (Var s) (PState m) = M.lookup s m
extractN (N i) = i
(N l) +: (N r) = N (l + r)
(N l) -: (N r) = N (l - r)
(N l) *: (N r) = N (l * r)
(N l) /: (N r) = N (l `div` r)


--need to implement a recursive descent parser

main = do
    ls <- getContents
    print "not implemented"
