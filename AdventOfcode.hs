import Data.List.Split
import Data.Hash.MD5

elevator s = 
   foldr (\c acc -> if (c == '(') then acc + 1 else acc -1) 0 s  

elevatorIndex (c:cs) floor i 
    | floor < 0  = i
    | c == '('   = elevatorIndex cs (floor + 1) (i +1)
    | c == ')'   = elevatorIndex cs (floor - 1) (i +1)
    | otherwise  = 0

boxSurfaceArea l w h = let
    a = l * w
    b = w * h
    c = h * l
    smallest = min (min a b) (min a c)
    in (2 * a) + (2 * b) + (2 * c) + smallest 

ribbonLength l w h = let
   a = 2 * (l + w) 
   b = 2 * (w + h) 
   c = 2 * (h + l)
   volume = l * w * h
   smallFace = min (min a b) (min a c) 
   in smallFace + volume
  
 
elvesWrappingPaper file f = do
   contents <- readFile file
   let ls = lines contents
   let parsed = map (map (\ln -> read ln :: Int) . splitOn "x") ls
   let area = foldr (\(a:b:c:[]) acc -> acc + (f a b c)) 0 parsed
   putStrLn (show area)
--should link this rather than paste...
contains :: Eq a => a -> [a] -> Bool
contains x [] = False
contains a (x:xs) = (a == x) || (contains a xs)

distinct ls =
    foldr (\x acc -> if (contains x acc) then acc else x:acc ) [] ls

walkGrid ('v':cs) ((x,y):ls) = walkGrid cs ((x, y-1):(x,y):ls) 
walkGrid ('<':cs) ((x,y):ls) = walkGrid cs ((x-1, y):(x,y):ls)
walkGrid ('>':cs) ((x,y):ls) = walkGrid cs ((x+1, y):(x,y):ls)
walkGrid ('^':cs) ((x,y):ls) = walkGrid cs ((x, y+1):(x,y):ls)
walkGrid [] ls = ls
walkGrid path start = start

santaGrid file = do
    path <- readFile file
    let visited = walkGrid path [(0,0)] 
    let count = length $ distinct visited
    putStrLn (show count)

roboSantaGrid file = do
    path <- readFile file
    let (santa, robo) = foldr (\(c, i) (l, r)-> if(even i) then (c:l, r) else (l, c:r)) ([],[]) $ zip path [0..]
    let sV = distinct $  walkGrid santa [(0,0)] 
    let rV = distinct $  walkGrid robo [(0,0)]
    let count = length $ distinct (sV ++ rV)
    putStrLn (show count)

hashCheck key prefix = let
    pl = length prefix
    in head $ filter (\(i, s) -> (take pl s) == prefix) $ map (\i -> (i, md5s $ Str $  key ++ (show i)) )  [0..]

niceString s = let
    vowelCount = foldr (\c acc-> if (c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u') then acc +1 else acc) 0 s
    (c, double) = foldr (\c (a, found) -> if (found || a == c) then (a, True) else (c, found)) ('_', False) s
    badList = "ab":"cd":"pq":"xy":[]
    clean = [] == ( filter (\a -> contains a badList) $ map (\(l,r) -> l:r:[]) $ zip s $ tail s)
    in vowelCount >= 3 && double && clean

newNiceString s = let
    pairs = map (\(a,b)-> a:b:[]) $ zip s $ tail s
    pred (found, rest) s = if (found || (length rest == 0)) then (found, []) else (contains s $ tail rest, tail rest)
    (pairRepeats, ls) = foldl (\acc s -> pred acc s ) (False, tail pairs) pairs 
    repeats = foldr (\(a,b) acc -> if(acc) then acc else a == b) False $ zip s (tail $ tail s)
    in repeats && pairRepeats

niceList file rules = do
    contents <- readFile file
    let ls = lines contents
    let niceCount = length $ filter(\s -> rules s) ls
    putStrLn (show niceCount)
   
 
