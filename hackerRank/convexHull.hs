import Text.Printf
import qualified Data.Tuple                 as T
import qualified Data.List                  as L

type Point = (Double, Double)

solve :: [Point] -> Double
solve points = let
    h = hull points
    in foldl (\a (l,r)-> a + (pointDist l r)) 0 $ zip h (tail h)

pointDist :: Point -> Point -> Double
pointDist (x,y) (x', y') = sqrt ((x - x')^2 + (y - y')^2)

hull :: [Point] -> [Point]
hull [] = []
hull [a] = [a]
hull as = lower ++ upper
    where 
        sorted = L.sort as
        lower = chain sorted
        upper = chain (reverse sorted)

chain :: [Point] -> [Point]
chain = f [] where
    f :: [Point] -> [Point] -> [Point]
    f acc@(r:r':rs) (x:xs) 
        | clockwise r' r x  = f (r':rs) (x:xs)
        | otherwise         = f (x:acc) xs
    f acc (x:xs)            = f (x:acc) xs
    f acc []                = reverse $ tail acc

clockwise :: Point -> Point -> Point -> Bool
clockwise o a b = (a `sub` o) `cross` (b `sub` o) <= 0

cross :: Point -> Point -> Double
cross (x, y) (x', y') = x * y' - x' * y

sub :: Point -> Point -> Point
sub (x, y) (x', y') = (x - x', y - y')

main = do
  n <- readLn :: IO Int
  content <- getContents
  let  
    points = map (\[x, y] -> (x, y)). map (map (read::String->Double)). map words. lines $ content
    ans = solve points
  printf "%.1f\n" ans
