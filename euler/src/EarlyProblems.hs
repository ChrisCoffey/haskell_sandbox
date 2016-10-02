module EarlyProblems where

import qualified Info.EarlyProblems as EPI

import Data.Char (digitToInt, ord, isAlpha)
import Data.Function (on)
import Data.List (intersect, find, maximumBy, permutations, sort, (\\), tails, nub, sortBy)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

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
primes :: [Integer]
primes = filter isPrime [2..]

primeFactors :: Integer -> [Integer]
primeFactors n = let 
    maxFactor = maximum possibleFactors
    possiblePrimes = takeWhile (< maxFactor) primes
    in possiblePrimes `intersect` possibleFactors
    where possibleFactors = filter ( `divisibleBy` n) [2..(fromIntegral . floor . sqrt $ fromInteger n)]

triangleNumbers :: [Integer]
triangleNumbers = f <$> [1..]
    where f n = (n * (n + 1)) `div` 2

palindromeNumber :: Integer -> Bool
palindromeNumber n =  show n == (reverse $ show n)

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

problem26 :: Integer
problem26 = undefined

--problem27 :: Int
problem27 = maximumBy (compare `on` (length . uncurry quadPrimes)) coefs 
    where coefs = [(a,b)| a <- [-999..999], b <- filter isPrime [0..999]]
          f a b n =  (n^2) + (a * n) + b
          quadPrimes :: Integer -> Integer -> [Integer]
          quadPrimes a b = takeWhile isPrime (f a b <$> [0..])
