import Control.Monad
import Data.List (group,sort, sortBy, insertBy)
import Data.Ord (comparing)
import qualified Data.Map as Map
import System.Random hiding (split)

--1
last ls = (head . reverse) ls
--2
secondToLast ls = (head . tail . reverse) ls
--3
nth ls n = head (drop (n - 1) ls)
--4
size ls = foldl (\acc e -> acc + 1) 0 ls
--5
revList ls = foldl (\acc e -> e : acc) [] ls
--6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome ls = all (\(x,y) -> x == y )(zip ls (reverse ls)) 
--7
data NestedList a = Elem a | List [NestedList a]
-- peel off the head, add it to the list if it's an element. Otherwise process
-- the list from the head and work your way down
flattenL:: NestedList a -> [a]
flattenL ls = reverse (combine [] ls) where
    combine acc (Elem x) = x : acc
    combine acc (List []) = acc
    combine acc (List (x:xs)) = combine (combine acc x) (List xs)
--8
compressL (x:xs)= 
    let (r, b) = foldl (\(as, a) e -> if (e == a) then (as, a) else (e:as, e)) ([x], x)  xs
    in reverse r
--9
pack (l:ls) = reverse (rec [] [l] ls) where
    rec acc as [] = as:acc
    rec acc as [x] = (x:as):acc
    rec acc as (b:bs) = if ((head as) == b) then rec acc (b:as) bs else rec (as:acc) [b] bs
--10
runLengthEncode ls = map (\xs -> (size xs, head xs)) (pack ls)
--11
data RLE a = Mult Int a | Sing a deriving (Show)
encodeModified :: Eq a =>  [a] -> [RLE a]
encodeModified ls = map (\(i, e) -> if(i == 1) then Sing e else Mult i e) $ runLengthEncode ls
--12
decodeRLE ls = concatMap (\l -> f l) ls where
    f (Sing x) = [x] 
    f (Mult i x) = replicate i x
--13
directRLE:: Eq a => [a] -> [RLE a]
directRLE (x:xs) = reverse $ map clean $ foldl f [(1, x)] xs where
    clean (1, a) = Sing a
    clean (i, a) = Mult i a
    f (h@(i, a):as) e = if (e == a) then (i + 1, a):as else (1, e):h:as
--14
duplicate xs = foldr (\ a acc -> a:a:acc) [] xs
--15
duplicateN xs n = concat $ foldr (\ a acc -> (replicate n a) : acc) [] xs
--16
dropEvery xs n = foldr (\ (a,i) acc -> if i `mod` n == 0 then acc else a:acc) [] $ zip xs [1..]
--17
split xs n = (take n xs, drop n xs)
--18
slice xs i k = take (k - i + 1) $ drop (max (i -1) 0) xs
--19
rotate xs n 
    | n > 0 = (drop n xs) ++ (take n xs)
    | otherwise = let
        dropsize = n + ( size xs)
        in (drop dropsize xs ) ++ (take dropsize xs)
--20
removeAt xs n = let
    h = take (n - 1) xs
    t = drop (n - 1) xs
    item = head t
    in (item, h ++ (tail t))
--21
insertAt elem xs n = let
    (l, r) = split xs (n - 1)
    in l ++ (elem : r)
--22
range start end = build start (end - start) where
    build s 0 = [s]    
    build s e = [s] ++ (build (s + 1) (e - 1))
--23
rndSample :: (RandomGen g) => [a] -> Int -> g -> (g, [a])
rndSample _ 0 g = (g, [])
rndSample [] _ g = (g, [])
rndSample xs i g = rndSample ys (i - 1) gg where
    (n, gg) = randomR (0, (length xs) -1) g
    (e, ys) = removeAt xs n

rndSample' xs n = do
    gen <- getStdGen
    return $ take n [ xs !! x | x <- randomRs (0, (length xs) - 1) gen]
--These work but are imperfect because the sequences generated seem to be deterministic
--24
rndSelect n m = do
    gen <- getStdGen 
    return $ take n [ x | x <- randomRs (1, m) gen]
--25
permute:: [a] -> IO [a]
permute [] = return []
permute (h:ls) = do
    i <- randomRIO (0, (length ls) - 1)
    xs <- permute ls 
    return $ let (a, b) = splitAt i xs 
             in (h:b) ++ a
--26
combo xs 0 = [[]]
combo xs n = [xs!!i : x| i <- [0..(length xs) -1], x <- combo (drop (i + 1) xs) (n -1)]

contains :: Eq a => a -> [a] -> Bool
contains x [] = False
contains a (x:xs) = (a == x) || (contains a xs)

forAll :: Eq a => (a -> Bool) -> [a] -> Bool
forAll _ [] = True
forAll f (x:xs) = (f x) && (forAll f xs)

except :: Eq a => [a] -> [a] -> [a]
except [] _ = []
except _ [] = []
except xs ys = foldr (\e acc -> if(contains e xs) then acc else e:acc) [] ys
--27
vGroups :: Eq a => [a] -> [Int] -> [[[a]]]
vGroups xs ns = build xs ns where
    build _ [] = []
    build ys (n:ns) = let
        block = combo ys n
        in map (\elem -> elem : concat (build (except elem ys) ns)) block
--28
partition :: Ord a => [a] -> a -> ([a], [a], [a])
partition ls a = let
    lessThan = filter (\v -> v < a) ls
    same = filter (\v -> v == a) ls
    greater = filter (\ v -> v > a) ls
    in (lessThan, same, greater)

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (a:as) = let
    (ls, s, gs) = partition as a
    in (quicksort ls) ++ [a] ++ s ++ (quicksort gs)

lengthSort :: Ord a => [[a]] -> [[a]]
lengthSort ls = let
    idxed = map (\a -> (length a, a)) ls
    sorted = quicksort idxed
    in map (\(i,v) -> v) sorted

--this certainly already exists, but I need it without access to docs :(
sliding :: [a] -> Int -> [[a]]
sliding [] _ = []
sliding ls i = let
    slice = take i ls
    rest = drop i ls
    in slice : (sliding rest i)
--lengthFrequencySort :: Ord a => [[a]] -> [[a]]
lengthFrequencySort ls = let
    idxed = map (\a -> (length a, a)) ls
    lengthGroups = foldr (\(i, xs) acc -> Map.insertWith (++) i xs acc) Map.empty idxed
    cleaned = map (\(i, x) -> (i, (sliding x i))) $ Map.toList lengthGroups 
    prepared = map (\(_, ls) -> (length ls, ls)) cleaned
    sorted = quicksort prepared
    in sorted 
--31 the lazy way
isPrime :: Int -> Bool
isPrime n = let
    m = floor $ sqrt $ fromIntegral n
    hasDivisor = any (\x -> n `mod` x == 0) [2..m]
    in not hasDivisor
--32
gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' a b = gcd b (a `mod` b)
--33
coPrime :: Int -> Int -> Bool
coPrime a b = 1 == gcd a b
--34
totientPhi :: Int -> Int
totientPhi a = length [n | n <- [1 .. a], coPrime a n]
--35
primeFactors :: Int -> [Int]
primeFactors a 
   | isPrime a  = a : []
   | otherwise  = (head ls) : (primeFactors m)
   where
        x = floor $ sqrt $ fromIntegral a
        ls =  [n | n <- [2 .. x], isPrime n, a `mod` n == 0 ]
        m = a `div` (head ls)
--36
primeFactorsMult a = 
    map (\ls -> (head ls, length ls)) . group . primeFactors $ a
--37
totient' a = 
   product $  map (\(p, m) -> (p - 1) * p ^ (m -1)) . primeFactorsMult $ a
--38
--39 very slow for large ranges
primes start end = filter isPrime $ filter (\i -> i `mod` 2 == 1) [start..end]
--40
goldbach n = let
    p = [x | x <- primes 3 n, isPrime (n - x)]
    in (head p, n - head p) 
--41
listGoldbachs a b = map (\x -> goldbach x)  [n | n <- [a..b], even n]

listGoldbachs' a b x = 
    filter (\(m,n) -> m > x && n > x ) $ listGoldbachs a b
--46
_and a b = a && b
_or a b = a || b
_nand a b = not (and' a b)
_nor a b = not (or' a b)
_xor a b = and' (not (and' a b)) (or' a b)
_impl a b = if (a) then b else True
_equiv a b = a == b

truthTable :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
truthTable pred = let
    args = [(x, y) | x <- [0..1], y <- [0..1]]
    bools = map (\(x, y) -> (x == 1, y == 1)) args
    in map (\(a,b) -> (a, b, pred a b)) bools
--47
infixl 7 `and'`
a `and'` b = _and a b
infixl 6 `or'`
a `or'` b = _or a b
a `nand'` b = _nand a b
a `nor'` b = _nor a b
a `xor'` b = _xor a b
infixl 5 `impl'`
a `impl'` b = _impl a b
infixl 4 `equiv'`
a `equiv'` b = _equiv a b

--48
distinct ls =
    foldr (\x acc -> if (contains x acc) then acc else x:acc ) [] ls

truthTableLen :: Int -> ([Bool] -> Bool) -> [[Bool]]
truthTableLen len pred = let
    args = distinct $ combo (concat $ map (\_ -> [True, False]) [1..len]) len
    res = map (\arg -> arg ++ [pred arg]) args    
    in res
--49
grayCode n = build n where
    f a = map(\s -> a++s)
    build 1 = ["0", "1"]
    build x = f "0" (build (x-1) ) ++ f "1" (reverse $ build (x -1))    
--50
data  BinaryTree a = Leaf a | Branch (BinaryTree a) (BinaryTree a) deriving Show

huffman :: (Ord a, Ord b, Num b) => [(a, b)] -> [(a, String)]
huffman ls = sortBy (comparing fst) $ serialize $ makeTree $ sortBy (comparing fst) $ 
        [(weight, Leaf v)| (v, weight) <- ls]
    where makeTree [(_, t)] = t
          makeTree ((weight, a):(weight',b):rest) = 
              makeTree $ insertBy (comparing fst) (weight + weight', Branch a b) rest
          serialize (Branch l r) = 
              [(c, '0':code) | (c, code) <- serialize l] ++ [(c, '1':code)| (c, code) <- serialize r]
          serialize  (Leaf c) = [(c, "")]

