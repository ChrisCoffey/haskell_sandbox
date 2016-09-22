module EarlyProblems where

import Data.List (intersect, find)

divisibleBy :: Integer -> Integer -> Bool
divisibleBy x y = y `mod` x == 0

multiplesOfNs :: [Integer] -> [Integer]
multiplesOfNs xs = filter (\x -> any (\f -> f x) $  map divisibleBy xs) [1..]

fibonacciS :: [Integer]
fibonacciS = 0 : 1 : zipWith (+) fibonacciS (tail fibonacciS)

isPrime :: Integer -> Bool
isPrime n  = not (any (`divisibleBy` n) possibleFactors)
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
    where bigN = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
          i x= (read [x]):: Integer

problem9 :: Integer
problem9 = head [a*b*c | a <- [1..996], b <- [(a+1)..997], c <- [(b+1)..998], a+b+c == 1000 && pythagTriplet a b c]

problem10 :: Integer
problem10 = sum . takeWhile (< 10) $ primes
