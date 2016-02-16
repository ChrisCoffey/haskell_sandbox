import Control.Monad
import Data.Functor
import qualified Data.Map           as M

type Var    = String
type N      = Integer
data Oα     = Plus | Minus | Mult | Div
data Oβ     = And | Or
data Oρ     = Gt | Lt
data AExp   = ExpAV Var | ExpAN N | ExpAO Oα AExp Oα | ExpAP AExp
data BExp   = T | F | BExpB BExp Oβ BExp | BExpR BExp Oρ BExp | BExpP BExp 
data Stmt   = ASSIGN Var AExp | SEQ Stmt Stmt | IF BExp Stmt Stmt | WHILE BExp Stmt

type PState = M.Map String N
type Prog   = M.Map Int Stmt -- not thrilled with representing a program as a simple sequence of statements

--need to implement a recursive descent parser
--need to implement the interpreter for the language above

main = do
    ls <- getContents
    print "not implemented"
