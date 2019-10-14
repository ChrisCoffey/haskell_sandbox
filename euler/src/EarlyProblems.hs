module EarlyProblems where

import qualified Info.EarlyProblems as EPI

import Data.Bits (testBit)
import Data.Char (digitToInt, ord, isAlpha)
import Data.Function (on)
import Data.List (intersect, partition, find, maximumBy, permutations, sort,
    (\\), tails, nub, sortBy, nubBy, insert, isPrefixOf, sortOn, groupBy,
    subsequences)
import Data.Monoid ((<>))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Foldable (foldl')
import Data.Ord (comparing)
import Data.Ratio (Rational, (%))
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

import Debug.Trace

possibleFactors :: Integer -> [Integer]
possibleFactors x = [2..(fromIntegral . floor . sqrt $ fromInteger x)]

divisibleBy :: Integer -> Integer -> Bool
divisibleBy x y = y `mod` x == 0

multiplesOfNs :: [Integer] -> [Integer]
multiplesOfNs xs = filter (\x -> any (\f -> f x) $  map divisibleBy xs) [1..]

fibonacciS :: [Integer]
fibonacciS = 0 : 1 : zipWith (+) fibonacciS (tail fibonacciS)

isPrime :: Integer -> Bool
isPrime n
    | n <= 0    = False
    | otherwise = not (any (`divisibleBy` n) possibleFactors)
    where possibleFactors = [2..(fromIntegral . floor . sqrt $ fromInteger n)]

--naive approach
-- A sieve will be much more efficient
slowPrimes :: [Integer]
slowPrimes = 2 : filter isPrime [3,5..]

--Walk a pair of sorted lists & efficiently subtract the sencond from the first
minus :: Ord a => [a] -> [a] -> [a]
minus (x:xs) (y:ys) =
    case compare x y of
        LT -> x: minus xs (y:ys)
        GT -> minus (x:xs) ys
        EQ -> minus xs ys
minus xs _ = xs --Covers the empty list case b/c it has already passed the pattern above

union :: Ord a => [a] -> [a] -> [a]
union (x:xs) (y:ys) =
    case compare x y of
        LT -> x: union xs (y:ys)
        GT -> y: union (x:xs) ys
        EQ -> x: union xs ys
union xs _ = xs

primes :: [Integer]
primes = 2 : minus [3..] (foldr (\p r-> p*p : union [p*p+p, p*p+2*p..] r) [] primes)

-- Produces a list of the lower factors (meaning x*y = z), if x < y, then x is the lower factor
primeFactors :: Integer -> [Integer]
primeFactors n =
    (concatMap primeFactors np) ++ pfs
    where
        factors = filter (\x -> divisibleBy x n) $ possibleFactors n
        (pfs, np) = partition isPrime factors

triangleNumbers :: [Integer]
triangleNumbers = f <$> [1..]
    where f n = (n * (n + 1)) `div` 2

triangleNumber :: Int -> Int
triangleNumber n = n * (n+1) `div` 2

palindromeNumber :: Integer -> Bool
palindromeNumber n =  reverseNumber 10 n == n

reverseNumber :: Integer -> Integer -> Integer
reverseNumber base n = rev n 0
    where
    rev :: Integer -> Integer -> Integer
    rev x a
        | x > 0     = rev (x `div` base) (base * a + (x `mod` base))
        | otherwise = a

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

divisibleByAll :: [Integer] -> Integer -> Bool
divisibleByAll xs x = all (`divisibleBy` x) xs

sumOfSquares :: [Integer] -> Integer
sumOfSquares xs = sum $ zipWith (*) xs xs

squareOfSums :: [Integer] -> Integer
squareOfSums xs = let
    x' = sum xs
    in x' * x'

window :: Int -> [a] -> [[a]]
window n xs
    | null $ drop n xs  = []
    | otherwise         = take n xs : window n (tail xs)

pythagTriplet :: Integer -> Integer -> Integer -> Bool
pythagTriplet a b c = (a < b  && b < c ) && (a*a + b*b == c * c)

type Grid = [[Integer]]
type Row =  [Integer]
--assumes an associative operator
gridProducts :: (Integer -> Integer -> Integer) -> Int -> Grid -> [Grid]
gridProducts f runLength rows = let
    xs = map (\x -> map (drop x) rows) [0..runLength-1]
    ys = map (`drop` rows) [0..runLength-1]
    zs = map (\x -> map (drop x) (drop x rows)) [0..runLength-1]
    backwards = map reverse rows
    as= map (\x -> map (drop x) (drop x backwards)) [0..runLength-1]
    in [computed xs, computed ys, computed zs, computed as]
    where computed :: [Grid] -> Grid
          computed = foldl1 computeGrid
          computeGrid :: Grid -> Grid -> Grid
          computeGrid = zipWith computeRows
          computeRows :: Row -> Row -> Row
          computeRows = zipWith f

factorCount :: Integer -> Integer
factorCount n = foldl f 0 [1..(fromIntegral . floor . sqrt $ fromInteger n)]
    where f acc x
            | divisibleBy x n = acc + 2
            | otherwise = acc

--This is very memory inefficient, there's likely an equation that's more efficient
collatzSeq :: [(Integer, Integer)]
collatzSeq = go M.empty 1 1 1
    where
        go :: M.Map Integer Integer -> Integer -> Integer -> Integer -> [(Integer, Integer)]
        go subs n m acc
            | n `M.member` subs = let
                    m'  = m + 1
                    tot = acc + (subs M.! n)
                    s'  = M.insert m tot subs
                    in (m, tot) : go s' (f m') m' 1
            | n == 1          = let
                    m' = m + 1
                    in (m, acc) : go (M.insert m acc subs) (f m') m' 1
            | otherwise = go subs (f n) m (acc + 1)
        f n
            | even n    = n `div` 2
            | otherwise = (3 * n) + 1

factorial :: Integer -> Integer
factorial n = product [1..n]

--central binomial coefficients form the center column of pascal's triangle!!
centralBinomialCoefficient :: Integer -> Integer
centralBinomialCoefficient n = factorial (2 * n) `div` (nFac * nFac)
    where nFac = factorial n

centralBinomialCoefficients :: [Integer]
centralBinomialCoefficients = map centralBinomialCoefficient [1..1000]

data PyramidBlock = Block {maxPath :: Integer }
type PyramidLayer = [PyramidBlock]

maximumPathPyramid ::[[Integer]] -> Integer
maximumPathPyramid ls = let
    rls     = reverse ls
    base    = head rls
    ls'     = reverse . tail $ rls
    baseLayer = map Block base
    in maxPath . head $ foldr makeLayer baseLayer ls'
    where
        makeLayer ::  [Integer] -> PyramidLayer -> PyramidLayer
        makeLayer raw prevLayer = let
            ridx = zip raw [0..]
            in foldr go [] ridx
            where f (Block l) (Block r) x = Block (x + (max l r))
                  go (x, i) layer = f (prevLayer !! i) (prevLayer !! (i + 1)) x : layer

properDivisors :: Integer -> [Integer]
properDivisors n =  1:actualFactors
    where actualFactors = nub . foldl (\a x-> x: (n `div` x):a) []  $ filter (`divisibleBy` n) $ possibleFactors n

areAmicable :: Integer -> Integer -> Bool
areAmicable x y = (sum $ properDivisors x) == (sum $ properDivisors y)

split :: Eq a => a -> [a] -> [[a]]
split a = f . foldr step ([],[])
    where
    f (as, acc) = as:acc
    step x (as, acc)
        |x == a         = ([], as:acc)
        | otherwise     = (x:as, acc)

stripOut :: Eq a => a -> [a] -> [a]
stripOut a = foldr step []
    where
    step x acc
        | x == a        = acc
        | otherwise     = x:acc

abundantNumbers :: [Integer]
abundantNumbers = filter isAbundant [1..]
    where isAbundant n = (sum $ properDivisors n) > n

reciprocalCycle :: Integer -> Integer
reciprocalCycle n =  head [x | x <- [1..], (10^x -1) `mod` n == 0 ]

distinctPowers :: Integer -> Integer -> [Integer]
distinctPowers a b = nub [x^y| x <- [2..a], y <- [2..b]]

digits :: Int -> [Int]
digits = fmap digitToInt . show

iDigits :: Integer -> [Integer]
iDigits = fmap (toInteger . digitToInt) . show

nthPowerSum :: Int -> [Int] -> Int
nthPowerSum n xs = sum $ (^n) <$> xs

nthPowerSumFast :: M.Map Int Int -> [Int] -> Int
nthPowerSumFast m xs = sum $ (m M.!) <$> xs

combinations :: [Int] -> Int -> [[Int]]
combinations _ 0        = [[]]
combinations [] _       = []
combinations (x:xs) n
    | x <= n            = (x:) <$> (combinations (x:xs) (n - x)) ++  (combinations (xs) n)
    | otherwise         = combinations (xs) n

overlappingDigits :: Eq b => (a -> [b]) -> a -> a -> Bool
overlappingDigits f l r = (f l \\ f r) /= (f l )

toInts :: [Int] -> Int -> (Int, Int)
toInts xs n = (a, b)
    where
    a = numify $ take n xs
    b = numify $ drop n xs

numify :: Num a => [a] -> a
numify = fst . foldr (\x (y, t)-> (y+(x*(10^t)), t+1)) (0,0)

intListToNum :: Num a => [a] -> a
intListToNum = foldl (\acc a -> a + (acc * 10)) 0

curiousFractions :: [Rational]
curiousFractions = [n % d | n <- [1..99], d <- [99,98..n], curious n d]
    where
    cancel :: Integer -> Integer -> Integer
    cancel n d = let
        n' = toInteger . numify $ (iDigits n \\ iDigits d)
        in if n' >= 1 then n' else 100000
    curious n d
        | n `mod` 10 == 0 && d `mod` 10 == 0 = False
        | otherwise = (cancel n d) % (cancel d n) == n % d && cancel n d < n

curiousNumbers :: [Integer]
curiousNumbers =  [n | n <- [3..(factorial 9)], curious n]
    where
    curious n = n == ( sum . map factorial $ iDigits n)

rotations :: [a] -> [[a]]
rotations xs = [drop n xs <> take n xs | n <- [0..length xs -1]]

digitRotations :: Integer -> [Integer]
digitRotations = fmap numify . rotations . iDigits

circularPrime :: Integer -> Bool
circularPrime = all isPrime . digitRotations

toBinary :: Integer -> [Integer]
toBinary n = dropWhile (== 0) $ foldl (\acc x-> if testBit n x then 1:acc else 0:acc) [] [0..127]

dropLeft :: Integer -> Integer -> Maybe Integer
dropLeft _ 0    = Nothing
dropLeft base n = Just $ n `div` base

dropRight :: Integer -> Integer -> Maybe Integer
dropRight _ 0 = Nothing
dropRight base n = (n `mod`) <$> maxBase
    where maxBase = (\i-> base ^ (i-1)) <$> find (\i-> n `div` (base ^ i) == 0) [1..]

truncations :: Integer -> Integer -> [Integer]
truncations base n = nub $ n : catMaybes (go s dropRight <> go s dropLeft)
    where
    s = Just n
    go Nothing _    = []
    go (Just x) f   = let
        x' = f base x
        in if x > 0 then (Just x) : go (f base x) f else go (f base x) f

isPandigital :: Int -> Bool
isPandigital n = isPandigital 9

isPandigitalN :: Int -> Int -> Bool
isPandigitalN len n =
    length ds == len && go ds [1..len]
    where
    ds = digits n
    go [] [] = True
    go [] _  = False
    go _  [] = False
    go (x:xs) ls = go xs (filter (/= x) ls)

powTenLength :: Int -> Int
powTenLength n = fromMaybe 0 $ find (\i -> n `div` (10 ^ i) == 0) [1..]

pandigitals :: Int -> [Int]
pandigitals len =  numify <$> permutations [1..len]

wordScore :: String -> Int
wordScore s = sum $ ( (\x -> x-64) . ord) <$> s

pentagonalNums :: [Int]
pentagonalNums =  pentagonalNum <$> [1..]

pentagonalNum :: Int -> Int
pentagonalNum n = n * ( (3 * n) -1 ) `div` 2

hexagonalNums :: [Int]
hexagonalNums = f <$> [1..]
    where f n = n * ((2 *n) -1)

hexagonalNum :: Int -> Int
hexagonalNum n = n * ((2*n) - 1)

isTriangular :: Int -> Bool
isTriangular t = let
    t' = fromIntegral t :: Double
    sq = sqrt ((8*t')+1)
    (_, remainder) = properFraction sq
    in remainder == 0

isPentagonal :: Int -> Bool
isPentagonal p = let
    p' = fromIntegral p :: Double
    sq = (sqrt ((24*p')+1) + 1) / 6
    (_, remainder) = properFraction sq
    in remainder == 0


pairs :: [a] -> [(a,a)]
pairs xs = f xs (tail xs)
    where
    f [] _  = []
    f (a:as) [] = f as (tail as)
    f (a:as) (b:bs) = (a,b):f (a:as) bs

--
-- Problems
--

problem1 :: Integer
problem1 = sum . takeWhile (< 1000) $ multiplesOfNs [3,5]

problem2 :: Integer
problem2 = sum . filter even . takeWhile (< 4000000) $ fibonacciS

problem3 :: Integer
problem3 = maximum $ primeFactors 600851475143

problem4 :: Integer
problem4 = maximum .  take 50 . filter palindromeNumber $ concatMap (\x -> map (* x) [999,998..500] ) [999,998..500]

problem5 :: Maybe Integer
problem5 = find (divisibleByAll [1..20]) [20,40..]

problem6 :: Integer
problem6 = squareOfSums [1..100] - sumOfSquares [1..100]

problem7 :: Integer
problem7 = primes !! 10001

problem8 :: Integer
problem8 = maximum . map (product . map i) $ window 13 bigN
    where bigN = EPI.problem8
          i x= (read [x]):: Integer

problem9 :: Integer
problem9 = head [a*b*c | a <- [1..996], b <- [(a+1)..997], c <- [(b+1)..998], a+b+c == 1000 && pythagTriplet a b c]

problem10 :: Integer
problem10 = sum . takeWhile (< 10) $ primes

problem11 :: Integer
problem11 = maximum . maximum . map maximum $ gridProducts (*) 4 grid
    where grid = EPI.problem11


problem12 :: Maybe Integer
problem12 = find ((> 500) . factorCount) triangleNumbers

problem13 :: String
problem13 = take 10 . show . sum $ EPI.problem13

problem14 :: Integer
problem14 = fst . maximumBy cmp . take 1000000 $ collatzSeq
    where cmp (a, al) (b, bl) = al `compare` bl

problem15 :: Integer
problem15 = centralBinomialCoefficient 20

--This is easy to brute force thanks to Integer, but there must be a better way
problem16 :: Int
problem16 = sum . map digitToInt . show $ 2 ^ 1000

problem17 :: Int
problem17 = sum . map (length . stringify) $ [1..1000]
    where stringify n
            | n == 0                = ""
            | n == 10               = "ten"
            | n == 11               = "eleven"
            | n == 12               = "twelve"
            | n == 13               = "thirteen"
            | n == 14               = "fourteen"
            | n == 15               = "fifteen"
            | n == 16               = "sixteen"
            | n == 17               = "seventeen"
            | n == 18               = "eighteen"
            | n == 19               = "nineteen"
            | n < 10                = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] !! (n -1)
            | n < 100 && n >= 20    = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"] !! ((n `div` 10) - 2) <> stringify (n `mod` 10)
            | n == 1000             = "one"<>"thousand"
            | otherwise             = "hundred" <> (if n `mod` 100 == 0 then "" else  "and") <> stringify (n `mod` 100) <> stringify (n `div` 100)

problem18 :: Integer
problem18 = maximumPathPyramid EPI.problem18

--concatMap is effectively flatMap from scala wrt List a
problem19 :: Int
problem19 =
    length . filter isMatch . drop 365 . zip weeks . concat . concatMap (\y -> map (month y) [1..12]) $ [1900..2000]
    where
    isMatch (1,1) = True
    isMatch _     = False
    weeks = cycle [2,3,4,5,6,7,1]
    month y m
        | m `elem` [4,6,9,11]      = [1..30]
        | m == 2 && isLeapYear y   = [1..29]
        | m == 2                   = [1..28]
        | otherwise                = [1..31]
    isLeapYear y = if y `mod` 100 == 0 then y `mod` 400 == 0 else y `mod` 4 == 0

problem20 :: Int
problem20 = sum . map digitToInt . show $ factorial 100

problem21 :: Integer
problem21 = sum . filter (\x-> f x <= 10000 && (f x /= x) && x == (factorSums M.! f x)) $ [1..10000]
    where factorSums = foldl (\m n-> M.insert n (f n) m) M.empty [1..10000]
          f = sum . properDivisors

problem22 :: IO Integer
problem22 = do
    names <-  split ',' . stripOut '"' <$> readFile "src/Info/names22.txt"
    let sNames = filter isAlpha <$> sort names
    let aScored = toInteger .  sum . map ((\x -> x - 64) . ord) <$> sNames
    let finalScore = zipWith (*) aScored [1..]
    pure $ sum finalScore

problem23 :: Integer
problem23 = let
    as = takeWhile (< 28123) abundantNumbers
    aSet = S.fromList as
    in sum . filter (\n -> valid n as aSet) $ [1..28123]
    where
    valid n xs aS = not . any (\x-> S.member (n - x) aS) . filter (< n) $ xs

problem24 :: [Int]
problem24 = head . drop 999999 . sort $ permutations [0..9]

problem25 :: Integer
problem25 = fst . head . dropWhile ( (< 3) . digits . snd) $ zip [1..] fibonacciS
    where digits = length . show

--result is the largest prime 'p' < 1k such that the cycle length = 'p'-1
problem26 :: Integer
problem26 =  head . filter (\n -> reciprocalCycle n == n - 1 ) . reverse . takeWhile (< 1000) $ primes

problem27 :: (Integer, Integer)
problem27 = maximumBy (compare `on` (length . uncurry quadPrimes)) coefs
    where coefs = [(a,b)| a <- [-999..999], b <- filter isPrime [0..999]]
          f a b n =  (n^2) + (a * n) + b
          quadPrimes :: Integer -> Integer -> [Integer]
          quadPrimes a b = takeWhile isPrime (f a b <$> [0..])

problem28 :: Integer
problem28 = sum $ spriralDiagonals 500
    where
    spriralDiagonals n = foldl f [1] [1..n]
        where f (x:xs) depth = let
                y = depth * 2
                in  (4*y) + x : (3*y)+ x : (2*y) + x: y + x:x:xs

problem29 :: Int
problem29 = length $ distinctPowers 100 100

problem30 :: Int
problem30 = sum $ filter (\n -> (\xs -> nthPowerSumFast m xs == n) $ digits n) [2..limit]
    where m = M.fromList . zip [0..] . fmap (^5) $ [0..9]
          limit = 6 * (9^5)

problem31 :: Int
problem31 = length $ combinations [1,2,5,10,20,50,100,200] 200

problem32 :: Int
problem32 = permSum 2 (ps 5) + permSum 2 (ps 4) + permSum 1 (ps 4) + permSum 1 (ps 5)
    where
    permSum n xs = sum . fmap (\(_,_,x)-> x) . nubBy sameProduct . filter pandigitalProd $ prods . flip toInts n <$> xs
    sameProduct (_, _, a) (_, _, b) = a == b
    ps n = nub $ take n <$> permutations [1..9]
    prods (a,b) = (a,b, a*b)
    xs = [1..9]
    pandigitalProd (a,b,c) = (== xs) . sort $ concatMap (fmap digitToInt . show) [a,b,c]

problem33 :: Rational
problem33 = product curiousFractions

problem34 :: Integer
problem34 = sum curiousNumbers

problem35 :: Int
problem35 = length . filter circularPrime $ takeWhile (< 1000000) primes

problem36 :: Integer
problem36 = sum . filter (isPalindrome . toBinary) $ filter palindromeNumber [1..1000000]

problem37 :: Integer
problem37 = sum . (\\ nonTruncatable) .
        filter noContainOne .
        filter (all isPrime . truncations 10) $
        takeWhile (< 1000000) primes
    where nonTruncatable = [2,3,5,7]
          noContainOne :: Integer -> Bool
          noContainOne   = notElem 1 . truncations 10

problem38 :: Int
problem38 = foldl f 918273645 [1..10000]
    where
    f largest x =  let
        x' = maximum . (\ls -> if null ls then [0] else ls) . filter isPandigital $ pandigitize x
        in max largest x'
    ns = [[1..n] | n <- [2..9]]
    concatNums = foldl (\a x -> a * (10 ^ powTenLength x) + x) 0
    pandigitize :: Int -> [Int]
    pandigitize x = fmap (concatNums . fmap (* x)) ns

--Note this is terribly inefficient, should use something like Euler's formula (implment this if I run across pythag triples again)
problem39 :: Integer
problem39 = fst . maximumBy (\(_, l) (_,r) -> l `compare` r ) $ M.toList accum
    where triplets = [a+b+c| a <- [1..996], b <- [(a+1)..997], c <- [(b+1)..998], pythagTriplet a b c, (a + b + c) <= 1000]
          accum = foldl (\acc p -> M.insertWith (+) p 1 acc) M.empty triplets

problem40 :: Int
problem40 = product $ (ds !!) <$> ixs
    where ds = take 1000001 . concat $ digits <$> [1..]
          ixs =  (\x -> 10 ^ x -1 ) <$> [0..6]

problem41 :: Int
problem41 = maximum $ concat px
    where px = filter (isPrime . toInteger) . pandigitals <$> [1..9]

problem42 :: Int
problem42 = length . filter (`S.member` ts) $ wordScore <$> EPI.problem42
    where
    ts = S.fromList . take 1000 $ fromIntegral <$> triangleNumbers

problem43 :: Int
problem43 =
    sum . map intListToNum . algo $ zeroNinePandigitals
    where
    algo :: [[Int]] -> [[Int]]
    algo xs = filter (condition . trigrams) xs

    zeroNinePandigitals :: [[Int]]
    zeroNinePandigitals = permutations [0..9]

    trigram :: [Int] -> [Int]
    trigram = take 3

    trigrams :: [Int] -> [Int]
    trigrams [_, _, _] = []
    trigrams (_:rest) = intListToNum (trigram rest) : trigrams rest

    condition :: [Int] -> Bool
    condition [a, b, c, d, e, f, g] =
        a `mod` 2 == 0 &&
        b `mod` 3 == 0 &&
        c `mod` 5 == 0 &&
        d `mod` 7 == 0 &&
        e `mod` 11 == 0 &&
        f `mod` 13 == 0 &&
        g `mod` 17 == 0
    condition xs = trace (show xs) False


problem44 :: Maybe Int
problem44 = (\(a,b)-> b - a) <$> find (\(a,b)-> pDiff a b && pSum a b) px
    where
    pSum x y = x + y `elem` ps
    pDiff x y = (max x y) - (min x y) `elem` ps
    ps = take 4000 pentagonalNums
    px = sortBy (\(a,b) (x,y)-> compare (x-y) (a-b)) $ pairs ps

problem45 :: Maybe Int
problem45 = find (\x -> isTriangular x && isPentagonal x) $ hexagonalNum <$> [144..]

problem46 :: Maybe Integer
problem46 =
    find check composites
    where
        check x = let
            dLT = S.filter (< x) doubleSquares
            g [] = True
            g (a:rest) = if S.member (x-a) tenKPrimes
                         then False
                         else g rest
            in g (S.toList dLT)
        doubleSquares = S.fromList . take 10000 $ floor . (* 2) . (** 2) <$> [1..]
        tenKPrimes = S.fromList $ take 10000 primes
        composites = filter (`S.notMember` tenKPrimes) [3,5..]

-- Boneheaded brute force solution
problem47 :: [Integer]
problem47 =
    consecutivePrimes [644..]
    where
    cond x = not . null $ [ fc | fc <- distinctPrimeFactors x, length (nub fc) == 4]
    consecutivePrimes (a:b:c:d:rest)
        | cond a && cond b && cond c && cond d = [a,b,c,d]
        | otherwise = consecutivePrimes (b:c:d:rest)

    distinctFactors x = let
        lowerFactors = filter (`divisibleBy` x) $ possibleFactors x
        in [[f, x `div` f] | f <- lowerFactors]

    distinctPrimeFactors :: Integer -> [[Integer]]
    distinctPrimeFactors x = let
        factorChains = distinctFactors x
        expandChain xs = let
            (pfs, np) = partition (`S.member` pSet ) xs
            subFactors = concatMap distinctPrimeFactors np
            in if null subFactors
            then [pfs]
            else (<>pfs) <$> subFactors
        in concatMap expandChain factorChains

    pSet = S.fromList $ take 10000 primes

problem48 :: Integer
problem48 = (`mod` 10000000000) . sum $ selfPower <$> take 1000 [1..]
    where
    selfPower :: Integer -> Integer
    selfPower n = n ^ n

problem48' :: Integer
problem48' = foldl' modSum 0 $ selfPower <$> take 1000 [1..]
    where
    selfPower :: Integer -> Integer
    selfPower n = n ^ n

    -- modular arithmetic, very useful for finding the tails of large products/sums!
    modSum a b = ((a `mod` m) + (b `mod` m)) `mod` m
    m = 10000000000

problem49 :: [[Int]]
problem49 = filter isArithmeticSequence .
    concat .
    map (map sort. subsequences . map fst) .
    filter ((<=) 3 . length ) .
    groupBy (\a b -> snd a == snd b ) .
    sortOn snd .
    map (withDigits . fromInteger) .
    takeWhile (< 10000) $ primes
    where
       withDigits x =  (x, numify . sort $ digits x)
       isArithmeticSequence (a:b:c:[]) = b-a == c-b
       isArithmeticSequence (a:b:c:rest) =
           b-a == c-b && isArithmeticSequence (b:c:rest)
       isArithmeticSequence _ = False

problem50 :: [Integer]
problem50 = primesBelow
    where
        cap = 1000
        primesBelow = reverse $ takeWhile (< cap) primes

