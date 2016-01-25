import qualified Data.Map                   as M
import Data.Maybe

type Graph a = Map a [a]

dGraphAdd :: a -> a -> Graph a -> Graph a

dGraphFromList :: [(a, a)]-> Graph a
dGraphFromList ls = 

-- find the node in question
path :: a -> a -> Graph a -> Maybe [a]
path start end graph =  f [start] where
    f [] = []
    f (a:rest) = 
    
