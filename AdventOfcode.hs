{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Map as M
import qualified Data.Set as S (Set, member, fromList, insert, union) 
import Data.List.Split (splitOn)
import Data.List.Utils hiding (contains)
import Data.List (union, isInfixOf, sort, permutations, minimumBy, maximumBy, nub, group)
import Data.Ord (comparing)
import Data.Hash.MD5
import Data.Matrix
import Data.Bits
import Data.Char

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
  
differenceL as bs = 
    foldr (\e acc -> if(contains e bs) then acc else (e:acc)) [] as

symDifferenceL as bs = 
    (differenceL as bs) ++ (differenceL bs as)

gm :: Matrix Int -> (Int, Int) -> Int
gm m (x,y) = getElem x y m

data Act = On | Off | Toggle deriving Show

lightsGrid file = do
    contents <- readFile file
    let ls = lines contents
    let makePointRange s = map (\(x:y:[])-> (read x :: Int, read y :: Int)) $ map (\pt -> split "," pt) $ split " through " s
    let cleaned = clean ls where
        clean [] = []
        clean (('t':'u':'r':'n':' ':'o':'n':' ':pts):rest) = (On, makePointRange pts) : (clean rest)
        clean (('t':'u':'r':'n':' ':'o':'f':'f':' ':pts):rest) = (Off, makePointRange pts): (clean rest) 
        clean (('t':'o':'g':'g':'l':'e':' ':pts):rest) = (Toggle, makePointRange pts): (clean rest) 
    let inst = map (\(i, (x,y):(x',y'):[])-> (i, [(x1 + 1,y1 + 1)| x1 <- [(x:: Int)..x'], y1 <- [(y :: Int)..y']])) cleaned
    let m = matrix 1000 1000 $ (\_ -> 0)
    let transformed = foldl (\acc (i,ls) -> case i of On     -> foldr (\pt acc'-> setElem 1 pt acc') acc ls
                                                      Off    -> foldr (\pt acc'-> setElem 0 pt acc') acc ls
                                                      Toggle -> foldr (\pt acc'-> setElem (1 `xor` ( gm acc' pt) ) pt acc') acc ls) m inst
    putStrLn (show $ sum $ toList transformed)

--did a couple in scala
miniSeq = 
    ["\"cyxdpkh\\\\\\\"\"", "\"\"", "\"abc\"", "\"aaa\\\"aaa\"", "\"\\x27\""]

lineSpace file = do
    contents <- readFile file
    let lns = lines contents
    let codeLengths = foldr (\ln acc -> acc + (length ln)) 0 lns
    let doEscapes = f where
            f ('\\':'\\':xs)    = ('\\':doEscapes xs)
            f ('\\':'"':xs)     = ('"':doEscapes xs)
            f ('\\':'x':x:y:xs) = ('!':doEscapes xs)
            f (x:xs)            = (x:doEscapes xs)
            f []                = []
    let escapeLengths = foldr (\ln acc-> acc + (length ln) -2) 0 $ map doEscapes lns
    putStrLn (show (codeLengths - escapeLengths ))

encodedLineSpace file = do
   contents <- readFile file
   let lns = lines contents
   let codeLengths = foldr(\ln acc-> acc + (length ln)) 0 lns
   let doEncode = e where
       e ('\\':xs)   = ('\\':'\\':doEncode xs)
       e ('\"':xs)   = ('\\':'\"':doEncode xs) 
       e (x:xs)      = (x:doEncode xs) 
       e []          = []
   let encodedLengths = foldr (\ln acc-> acc + (length ln) +2) 0 $ map doEncode lns
   putStrLn (show (encodedLengths - codeLengths))

data WEdge a = WEdge a a Int  deriving Show

instance (Eq a) => Eq (WEdge a) where
    WEdge x y z == WEdge a b c = x  == a && y == b && z == c

--note can swapping the compare provide a maximal spanning tree???
instance Eq a => Ord (WEdge a) where
    (WEdge _ _ w) `compare` (WEdge _ _ y) = y `compare` w

mst :: Ord a => ([WEdge a], [S.Set a]) -> WEdge a -> ([WEdge a], [S.Set a])
mst (es, sets) e@(WEdge a b w) = step $ extract sets where
    step (rest, Nothing, Nothing) = (e: es, S.fromList [a,b] :rest) 
    step (rest, Just as, Nothing) = (e: es, (S.insert b as) :rest)
    step (rest, Nothing, Just bs) = (e: es, (S.insert a bs) :rest)
    step (rest, Just as, Just bs) | as == bs = (es, sets) -- this indicates a cycle, so the edge is not added
                                  | otherwise = (e: es, (S.union as bs):rest)
    extract = foldr f ([], Nothing, Nothing) where
        f s (list, seta, setb) = let
            ls' = if S.member a s || S.member b s then list else s:list
            seta' = if S.member a s then Just s else seta
            setb' = if S.member b s then Just s else setb
            in (ls', seta', setb')
--turns out this was the wrong algorithm... this is the minimum spanning tree
shortestPath file = do
    contents <- fmap lines (readFile file)
    let parse = p where
        p xs = let 
            (start:rest:[]) = split " to " xs
            (end: weight:[]) = split " = " rest 
            in (start, (read weight :: Int), end)
    let clean = map parse contents
    let edges = map(\(s,w,e)-> WEdge s e w) clean
    let kruskal = foldl mst ([], []) . sort
    putStrLn (show $  kruskal edges)        
    let weights = sum . map(\(WEdge a b c)-> c) . fst
    putStrLn (show $ weights $ kruskal edges)

hamPath file = do
    contents <- fmap lines (readFile file)
    let parse = p where
        p xs = let 
            (start:rest:[]) = split " to " xs
            (end: weight:[]) = split " = " rest 
            in (start, (read weight :: Int), end)
    let clean = map parse contents
    let edges = map(\(s,w,e)-> WEdge s e w) clean
    let perms = permutations $ edges
    let ps = map(\ls -> take 7 ls) perms
    let path = f where
        f (x:[]) = True
        f ((WEdge _ x _):e@(WEdge a _ _):rest) = x == a && f (e:rest)
    let paths = filter path ps
    putStrLn (show $ length edges)

--my version
type Weight = Int
type Vert = String
type Edge = (Vert, Vert)
type GraphM = M.Map Edge Weight

vertices :: GraphM -> [Vert]
vertices = nub . map fst . M.keys

getEdges :: [Vert] -> [Edge]
getEdges vs = zip  vs $ tail vs

weights :: GraphM -> Edge -> Weight
weights = (M.!)

pathLength :: GraphM -> [Edge] -> Weight
pathLength g = sum . map (weights g)

--concat map basically flat map
-- :: not always required for stirng/char -> int
cleverRead = M.fromList . concatMap (f . words) . lines
    where f [a,_,b,_,d] = [((a,b), read d),((b,a), read d)]

hamPath' file = do 
    pathMap <- fmap cleverRead (readFile file)
    let vs = vertices pathMap
        perms = permutations vs
        edges = map getEdges perms
        ws = map (pathLength pathMap) edges
        weightedPaths = zip edges ws
    print $ maximumBy (comparing snd) weightedPaths
      
lookNSay ns 0 = ns 
lookNSay (x:ns) iter = let
    (a, ln, xs) = foldl (\(prev, ln, ls) c-> if(c == prev) then (prev, ln + 1, ls) else (c, 1, prev:(chr (48 +ln)):ls)) (x, 1, []) ns
    step = reverse $ a:(chr (48 +ln)):xs
    in lookNSay step (iter - 1)

--no wifi, but these functions certainly already exist
zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex [] = []
zipWithIndex (x:xs) = 
    reverse $ foldl (\(b@(p, _):a) c-> (p+1, c):b:a) [(0, x)] xs

pairs ls = 
    (>= 2) . length . filter ((>= 2) . length) . group $ ls

goodLetter [] = True
goodLetter (a:as) 
    | a == 'i' = False
    | a == 'l' = False
    | a == 'o' = False
    | otherwise = True

strait (a:b:c:rest) = a == succ b && b == succ c || strait (b:c:rest)
strait (a:b:[]) = False

nextPw ('z':b:rest)  = 'a': nextPw (b:rest)
nextPw ['z'] = ['a']
nextPw (x:rest) = (succ x): rest

legit s = pairs s && goodLetter s && strait s

makeSantaPw inStr = 
    map reverse $ filter legit (iterate nextPw (reverse inStr)) 

--bit of grep and awk
--grep -E '[0-9]{1,}' 12.in | sed 's/ //g;s/.*://g;s/\,//g;'| awk '{s+=$1} END {print s}'

-- not familiar enough with haskell to handle this. Did it in js
