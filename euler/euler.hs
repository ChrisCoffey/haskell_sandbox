module Euler
(
firstNMultiples,
largestPrimeFactor,
isPalindrome,
pythagTriplet
) where

import Data.Char
import Data.List
import Data.List.Split
import Data.Array

addLists = zipWith (+)

firstNMultiples :: Int -> Int -> [Int]
firstNMultiples n x = [a |  a <- [1..n], a `mod` x == 0]

problem1 :: Int -> Int
problem1 n = sum [a | a <- [1..n-1], a `mod` 3 == 0 || a `mod` 5 == 0]

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

problem2 n = sum $ takeWhile (<n) $ filter (even) fibs

primes = 2 : filter (null . tail . primeFactors) [3,5..]

factors :: Int -> [Int] -> [Int]
factors n (x:xs)
	| x^2 > n			= [n]
	| n `mod` x == 0	= x: factors (n `div` x) (xs)
	| otherwise 		= factors n xs

fastFactors :: Int -> [Int]
fastFactors n = factors n [1..end]
	where end = floor . sqrt $ fromIntegral n

largestPrimeFactor n = last (primeFactors n)

primeFactors n = factor n primes
  where
    factor n (p:ps) 
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      =     factor n ps

divisors n (x:xs)
	| x^2 >n         = [n]
	| n `mod` x == 0 = x: (n `div` x): divisors (n `div` x) xs
	| otherwise		 = divisors n xs

simpleDivisors :: Int -> [Int]
simpleDivisors x = 1  : x : filter((== 0) . rem x) [2 .. x `div` 2]


isPalindrome x = (show x) == (reverse $ show x)

problem4 = maximum $ [a * b | a <-[1..999], b <- [1..999], isPalindrome $ a * b]

problem5 = foldr1 lcm [1..20]

sumOfSquares xs = sum $ map (^2) xs

squareOfSums xs = (sum xs)^2

problem6 xs = (squareOfSums xs) - (sumOfSquares xs)

problem7 = last (take 10001 primes)

problem8 = do
	-- open the file as a string monad
	text <- readFile "euler/problem_8_Data"
	-- given the open file, we're going to write the largest product
	print . maximum . map product
		-- of a list we create by folding right
		. foldr (zipWith (:)) (repeat [])
		-- and taking the first 13 elements from the tail
		. take 13 . tails .map (fromIntegral . digitToInt)
		-- of the file which has been concatenated together
		. concat . lines $ text

pythagTriplet x y = (x^2 - y^2, 2 * x * y, x^2 + y^2)

triplets limit = [(a, b, c)| x <- [2..limit],
						   y <- [1..(x-1)],
						   let (a,b,c) = pythagTriplet x y,
						   a + b + c == limit]

problem9 = a * b * c
	where (a,b,c) = head . triplets $ 1000

sumOfPrimesLessThan limit = sum $ takeWhile (<limit) primes


-- Come back to problem 11 once I understand list manipulation better

triangle :: Int -> Int
triangle n = (n*(n+1)) `div` 2

triangles = map (triangle) [1..]


trianglesWithNFactors :: Int -> Int
trianglesWithNFactors n = head $ filter ((> n) . nDivisors) triangles
	where nDivisors x  = product $ map ((+1) . length) (group (primeFactors x))

strToIntList :: String -> [Int]
strToIntList s = map (read . (:"")) s

