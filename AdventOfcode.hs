{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Map as M
import qualified Data.Set as S (Set, member, fromList, insert, union) 
import Data.List.Split (splitOn)
import Data.List.Utils hiding (contains)
import Data.List (union, isInfixOf, sort, permutations, minimumBy, maximumBy, nub, group, sortBy, groupBy, findIndex)
import Data.Ord (comparing)
import Data.Hash.MD5
import Data.Matrix
import Data.Bits
import Data.Char
import Math.NumberTheory.Primes.Factorisation

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
--turns out this was the wrong algorithm for the problem. should have been ham path not mst
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

hammingPath makeMap file = do
    pathMap <- fmap makeMap (readFile file)
    let vs = vertices pathMap
        perms = permutations vs
        edges = map getEdges perms
        ws = map (pathLength pathMap) edges
        weightedPaths = zip edges ws
    print $ maximumBy (comparing snd) weightedPaths

travelMap = hammingPath cleverRead

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
--solved part 2 in plain js

myWeights = M.fromList [
    (("Alice", "A"), 0),
    (("David", "A"), 0),
    (("Carol", "A"), 0),
    (("Bob", "A"), 0),
    (("Eric", "A"), 0),
    (("George", "A"), 0),
    (("Frank", "A"), 0),
    (("Mallory", "A"), 0),
    (("A", "Alice"), 0),
    (("A", "David"), 0),
    (("A", "Carol"), 0),
    (("A", "Bob"), 0),
    (("A", "Eric"), 0),
    (("A", "George"), 0),
    (("A", "Frank"), 0),
    (("A", "Mallory"), 0)
    ]

dinnerParse = mappify . foldr (\(l, r) (ls, rs)-> (l:ls, r:rs)) ([], []) . map (f . words) . lines
    where dropDot = takeWhile (\c -> c /= '.')
          f [a,_,"gain",b,_,_,_,_,_,_,d] = (((a,dropDot d), read b:: Int), ((dropDot d, a), read b:: Int))
          f [a,_,"lose",b,_,_,_,_,_,_,d] = (((a,dropDot d), -(read b:: Int)), ((dropDot d, a), -(read b:: Int)))
          mappify (ls, rs) = M.unionWith (+) (M.fromList ls) ( M.union (M.fromList rs) myWeights)

directedWeighted :: String -> IO ()
directedWeighted file = do 
    ls <- readFile file
    let clean = dinnerParse ls
    print clean

reindeerParse :: String -> [(String, Int, Int, Int)]
reindeerParse = map (f . words) .  lines 
    where dropDot = takeWhile (\c -> c /= '.')
          f [n, _, _, s, _, _, d, _, _, _, _, _, _, r, _] = (n, read s, read d, read r)

toDistance x  (n, speed, durr, rest) = let
    totalTime = durr + rest
    cycles = ((x - durr) `div` totalTime) + 1
    rem = x - ( cycles * totalTime )
    in (n, rem + totalTime, durr, rest, cycles * (speed * durr))

reindeerRace :: String -> Int -> IO ()
reindeerRace file durration = do 
    ls <- readFile file
    let clean = reindeerParse ls
        transform = map (toDistance durration) clean
    print transform

--iterate from 0 -> n-1
--at each point, apply the score function to my list that applies the list
--each list is 0-n, with cells filled with either 0 or speed
-- for the range, accumulate everyone's scores & record the winner in the victory list
toRacer :: Int -> Int -> Int -> Int -> [Int]
toRacer x durr speed rest = 
    take x 
    . concatMap (\i -> if i == 0 then take durr (repeat speed) else take rest (repeat 0)) 
    . map (\i -> i `mod` 2) 
    $ [0..1000]

nextStep :: (Int, (String, [Int])) -> (Int, (String, [Int]))
nextStep (curr, (n, x:xs)) = (curr + x, (n, xs))
nextStep (curr, (n, [])) = (curr, (n, []))

scoreStep (winners, racers) = let
    thisStep = map nextStep racers
    ws = map (\(_, (n, _))-> n) . head . groupBy (\l r-> (fst l) == (fst r)) . reverse . sort $ thisStep
    in (ws ++ winners, thisStep)

reindeerGames :: String -> Int -> IO ()
reindeerGames file x = do
    ls <- readFile file
    let clean = reindeerParse ls
        ready:: [(String, [Int])] = map (\(n, s,d,r)-> (n, toRacer x d s r)) clean
        (winners, _) = foldl (\a _-> scoreStep a) ([], (map (\ls -> (0, ls)) ready)) [1..2503]
        steady = map (\ls -> (length ls, head ls)) . group . sort $  winners
    print steady 

data CookieIngredient = CookieIngredient {
    name :: String,
    capacity :: Int,
    durability :: Int,
    flavor :: Int,
    texture :: Int,
    calories :: Int } deriving (Show, Eq)

parseCookie = f where
    scrub = takeWhile (\c -> c /= ',')
    f [n,_, c, _, d, _, f, _, t, _, ca] = CookieIngredient {
        name = n,
        capacity = read (scrub c),
        durability = read (scrub d),
        flavor = read (scrub f),
        texture = read (scrub t),
        calories = read (scrub ca) }

--this could be so much faster if memoized up front
cookieScore ingredients = f where
    cap n = capacity  (ingredients !! n)
    dur n = durability (ingredients !! n)
    flv n = flavor  (ingredients !! n)
    txt n = texture  (ingredients !! n)
    cal n = calories (ingredients !! n)
    f (a,b,c,d) = let
        c' = max ((a * (cap 0)) + (b * (cap 1)) + (c * (cap 2)) + (d * (cap 3))) 0
        d' = max ((a * (dur 0)) + (b * (dur 1)) + (c * (dur 2)) + (d * (dur 3))) 0
        f' = max ((a * (flv 0)) + (b * (flv 1)) + (c * (flv 2)) + (d * (flv 3))) 0
        t' = max ((a * (txt 0)) + (b * (txt 1)) + (c * (txt 2)) + (d * (txt 3))) 0
        ca' = max ((a * (cal 0)) + (b * (cal 1)) + (c * (cal 2)) + (d * (cal 3))) 0
        in if ca' == 500 then (c', d', f', t', (a,b,c,d)) else (0,0,0,0, (a,b,c,d))

-- can solve this with a gradient search through the space
-- by starting with 25 of each, then subtracting from one and adding to each of the others
-- before sticking with the one that made the most difference in total score
bestCookie file = do 
    ls <- readFile file
    let cookies = map parseCookie . map words $ lines ls
        choices = [(a,b,c,100 - (a + b + c))| a<-[0..100], b<-[0..100-a], c<- [0..100-(a+b)]]
        best = maximum .  map (\e@(a,b,c,d, _)-> (a*b*c*d, e)) . map (cookieScore cookies) $ choices
    print best

parseAunt :: [String] -> (String, (String, Int), (String, Int), (String, Int))
parseAunt = f where
    f [_, n, a, a', b, b', c, c'] = let
        ai = read (takeWhile (\c-> c /= ',') a')
        bi = read (takeWhile (\c-> c /= ',') b')
        ci = read (takeWhile (\c-> c /= ',') c')
        in (n, (a, ai), (b, bi), (c, ci))

auntie = M.fromList [
    ("children:", (== 3)), 
    ("cats:", (> 7)), 
    ("samoyeds:", (== 2)), 
    ("pomeranians:", (< 3)), 
    ("akitas:", (== 0)),
    ("vizslas:", (== 0)),
    ("goldfish:", (< 5)),
    ("trees:", (> 3)),
    ("cars:", (== 2)),
    ("perfumes:", ( == 1))]

findMyAunt file = do 
    ls <- readFile file
    let aunts = map parseAunt . map words $ lines ls
        goodAunt = f where
            lookup = (M.!) auntie
            f (n, (a, ai), (b, bi), (c, ci)) = (lookup a ai) && (lookup b bi) && (lookup c ci)
        likely = filter goodAunt aunts
    print likely

eggnogStorage file n = do
    containers <-  fmap (sort . map (\i-> read i::Int) . lines ) $ readFile file
    let store = f where
            f conts amt 
                | amt == 0 = 1
                | conts == [] = 0
                | amt < 0 = 0
                | otherwise = (f (tail conts) amt) + (f (tail conts) (amt - (head conts)))
        res = store containers n
    print res

eggnogMinStorage :: String -> Int -> Int -> IO ()
eggnogMinStorage file n m = do
    containers <-  fmap (sort . map (\i-> read i::Int) . lines ) $ readFile file
    let store = f where
            f conts amt ac
                | amt == 0 = 1
                | conts == [] = 0
                | amt < 0 = 0
                | ac == 0 = 0
                | otherwise = (f (tail conts) amt ac) + (f (tail conts) (amt - (head conts)) (ac - 1))
        res = store containers n m
    print res

animatedLights file g = do 
    lights <- fmap (map (\l -> map(\c-> if c == '#' then True else False) l) . lines) $ readFile file
    let dims = length lights
        onLights = [(x,y)| x <-[0..dims-1], y <- [0..dims-1], (!! y) . (!! x) $ lights]
        corners = [(0,0), (0, 99), (99, 0), (99,99)]
        step ls = [head cs | cs <- grouped grid, on cs] where
            grid = nub $ (ls ++ corners) 
            validPt (x',y') = x' >= 0 && y' >= 0 && x' < dims && y' < dims
            grouped = group . sort . concatMap neighbors
            neighbors (x,y) = filter validPt [(x+x', y+y')| x' <- [-1..1], y'<- [-1..1], (x',y')/= (0,0)] 
            on [_,_,_] = True
            on [a, _] = a `elem` grid
            on _ = False
        res = nub . (++ corners) . head . reverse . take g $ iterate step onLights
    print $ length res 

reindeerCode file = do
    ls <- fmap lines $ readFile file
    let code = last ls
        listify = f where
            f (x@(k, _):xs) = (k, foldr (\(_, v) ac -> v:ac) [] (x:xs))
        transforms = map listify . groupBy (\(l,_) (r,_)-> l == r) .  sort . map (\[a,_,o]-> (a,o)) . map words $ filter (contains '=') ls
        mp = M.fromList transforms
        part1 = f where
            f xs [] _ = xs
            f xs (l:r:rest) n
                | M.member (l:[]) mp = ((++ xs) . map (\s-> (take n code) ++ s++(r:rest)) $ (M.!) mp (l:[]) ) ++ (f xs (r:rest) (n+1))
                | M.member (l:r:[])  mp = ((++ xs) . map (\s-> (take n code) ++ s++rest) $ (M.!) mp (l:r:[]) ) ++ (f xs rest (n+2))
                | otherwise = f xs (r:rest) (n +1)
--        part2 = g where
--            tokenized = 
--            g [] n = n
--            g (l:r:rest) n = 
        res1 = length . nub $ part1 [] code 0
--        res2
    print res1
--    print res2

intSqrt = floor . sqrt . fromIntegral
  
primes = filter ((== 0) . length . divisors) [1..] where
    divisors n = [x | x <- [2..(intSqrt n)], n `mod` x == 0]

primeSet = S.fromList $ take 1000 primes 

factors n = let
    xs = [(x)| x<-[1..(intSqrt n)], n `mod` x == 0] 
    fs = map (\x -> n `div` x) xs
    in nub $ xs ++ fs

presents:: Int -> (Int, Int)
presents n = head . filter (\(i, x)-> x >= n) . zip [1..] . map ( (* 10) . sum . factors) $ [1..]

presents2 n = head . filter (\(i,x)-> x >= n) . zip [1..] . map ((*11) . sum . filter (> (n -1) `div` 50 .factors)) $ [1..]


weapons = [(0,4,8),  (0,5,10), (0,6,25), (0,7,40), (0,8,74)]
armor = [(1,0,13), (2,0,31), (3,0,53), (4,0,75), (5,0,102), (0,0,0)] 
rings = [(1,0,20), (2,0,40), (3,0,80), (0,1,25), (0,2,50), (0,3,100)]

rpgStore bh ba bd = let
    joinItems = f where
        f l@(a,d,c) r@(a',d',c') = (a+a', d+d', c+c')
    combos = do
       w <- weapons
       a <- armor
       rs <- [] ++ (map (\x->[x]) rings) ++ (nub . map (sort . take 2) . permutations $ rings) 
       return (foldl1 joinItems (w:a:rs))
    fight = f where
        f (a,d,c) = ((bh-1) `div` (max 1 (d - ba))) <= (99 `div` (max 1 (bd - a)))
    in filter (not . fight) $ combos

--need to build the wizard fight system


data Machine = Machine { a :: Int, b :: Int } deriving (Show)
type Instructions = M.Map Int (MachineExec -> MachineExec)
data MachineExec = MachineExec {machine :: Machine, idx:: Int } deriving (Show)

hlf :: String -> MachineExec -> MachineExec
hlf "a" (MachineExec (Machine a' b') i) = MachineExec {machine = Machine { a = (a' `div` 2), b = b' }, idx = i +1 }
hlf "b" (MachineExec m i) = MachineExec {machine = Machine { a = (a m), b = (b m) `div` 2 }, idx = i +1 }

tpl :: String -> MachineExec -> MachineExec
tpl "a" (MachineExec m i) = MachineExec { machine = Machine { a = (a m) * 3, b = b m }, idx = i +1 }
tpl "b" (MachineExec m i) = MachineExec {machine = Machine { a = (a m), b = (b m) * 3 }, idx = i +1 }

inc :: String -> MachineExec -> MachineExec
inc "a" (MachineExec m i) = MachineExec { machine = Machine { a = (a m) + 1, b = b m }, idx = i +1 }
inc "b" (MachineExec m i) = MachineExec { machine = Machine { a = (a m), b = (b m) + 1 }, idx = i + 1}

jmp :: Int -> MachineExec -> MachineExec
jmp n (MachineExec m i) = MachineExec { machine = m, idx = i + n }

jie :: Int -> String -> MachineExec -> MachineExec
jie n "a" (MachineExec m i)
    | even (a m) = MachineExec {machine = m, idx = i + n}
    | otherwise = MachineExec {machine = m, idx = i + 1}
jie n "b" (MachineExec m i)  
    | even (b m) = MachineExec {machine = m, idx = i + n}
    | otherwise = MachineExec {machine = m, idx = i + 1}

jio :: Int -> String -> MachineExec -> MachineExec
jio n "a" (MachineExec m i)
    | (== 1)  (a m) = MachineExec {machine = m, idx = i + n}
    | otherwise = MachineExec {machine = m, idx = i + 1}
jio n "b" (MachineExec m i)  
    | (== 1) (b m) = MachineExec {machine = m, idx = i + n}
    | otherwise = MachineExec {machine = m, idx = i + 1}

--need to record the index of the instructions
--its important that this can be moved around quickly
parseInst :: String -> (MachineExec -> MachineExec)
parseInst ln = f $ words ln where
    f ["inc", reg] = inc reg
    f ["tpl", reg] = tpl reg
    f ["hlf", reg] = hlf reg
    f ["jmp", '+':n] = jmp (read n :: Int)
    f ["jmp", n] = jmp (read n :: Int)
    f ["jie", reg:',':[], '+':n ] = jie (read n:: Int) (reg:[])
    f ["jie", reg:',':[], n ] = jie (read n:: Int) (reg:[])
    f ["jio", reg:',':[], '+':n ] = jio (read n:: Int) (reg:[])
    f ["jio", reg:',':[], n ] = jio (read n:: Int) (reg:[])

runTheMachine file = do
    lns <- fmap lines $ readFile file
    let insts = M.fromList . zip [1..] . map parseInst $ lns
        start = MachineExec{ machine = Machine {a = 0, b = 0 }, idx = 1 }
        start' = MachineExec{ machine = Machine {a = 1, b = 0 }, idx = 1 }
        step = f where
            f me@(MachineExec m i) = (M.! i) insts $ me
        res = head . filter (\(MachineExec m i)-> i > 49) . iterate step $ start
        res' = head . filter (\(MachineExec m i)-> i > 49) . iterate step $ start'
    print res
    print res'

addToHeadL :: a -> [[a]] -> [[a]]
addToHeadL a (as:rest) = (a:as):rest
addToHeadL a [] = [[a]]

combinations :: [Int] -> Int -> [[Int]] 
combinations xs n =  f xs n [] where
    f rest amt elems
        | amt == 0 = elems
        | rest == [] = [] 
        | amt < 0 = [] 
        | otherwise = (f (tail rest) amt elems) ++ (f (tail rest) (amt - (head rest)) (addToHeadL (head rest) elems))

quantumWeights file = do
    lns <- fmap lines $ readFile file
    let weights = map (\i-> read i :: Int) lns
        sectionWeight = (`div` 4) . sum $ weights
        qe = product
        cs = combinations (reverse weights) sectionWeight
        possible = sortBy (comparing length) . filter (\l-> (length l) <= 6) $ cs
    print $ sectionWeight
    print $ minimum (map product possible)

weatherCode x y = let
   maxRow = x + y
   idx = (sum [1..(maxRow)]) + y
   f = (\n-> (n * 252533) `mod` 33554393) 
   in head . drop (idx) $ iterate f 20151125
