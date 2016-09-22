
doubleMe x = x + x

sumOfDoubles x y = doubleMe x + doubleMe y

-- the else statement in a haskell if is required because all functions must return a value. No Unit allowed here!
doubleSmaller x y = if x > y then doubleMe y else doubleMe x

-- expressions are code that returns a value. Things like 5, doubleMe & doubleSmaller are all expressions.
-- Functions cannot begin with uppercase letters. Camel case is fine, but the first character cannot be upper case.

-- Lists
-- Haskell lists are homogenous, meaning they always contain elements of the same type. No heterogeneous lists in this language!
-- ++ concats two lists, but requires traversal of the first list in order to add the second. : (cons) is instant because we're just creating a single reference swap
-- !! is the list extractor.
ls = [1,2,3,4,5,6,7,8,9,10]
seven = ls !! 6

-- Lists are compared in lexographic order if there are comparison functions defined on the list elements. This means head => tail, left => right
inList = 25 `elem` ls -- checks if 25 is in ls. Its not...

removeLowercase str = [c | c <- str, c `elem` ['A'..'Z']]

tup = (1,2,3)
pair = (1,2)
beginning = fst pair
end = snd pair

-- good examples
-- let triangles = [(a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10]]
-- let rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2] 
-- let rightTriangles2 = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]
--These examples demonstrate idea of reducing to a solution. Very useful technique.

-- Integer = unbounded Int, good for use with giant numbers
-- type variables are written in lower-case. types use uppercase for the first character, but variables are lower

-- In type signatures, everything to the left of => is a class constraint, while the right is the actual function definition
-- Typeclasses define behavior, not so much data
-- Enum = sequentially ordered data. I.e. enumerable
-- Bounded = a type with a max value

-- :t (*) :: Num a => a -> a -> a means the * function takes two values of a type in the class Num, then returns a single value of the same type

sevenPat :: (Integral a) => a -> String
sevenPat 7 = "Seven!!!! It's a lucky number."
sevenPat x = "Not so lucky are you"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * factorial (x - 1)

-- as patterns
firstChar :: String -> String
firstChar all@(x:xs) = "first character of " ++ all ++ " is " ++ [x] 

-- guards
max' :: (Integral a) => a -> a -> a
max' x y
  | x > y = x
  | otherwise = y

-- where binding
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell height weight 
  | bmi <= 18.5 = "Too Skinny"
  | bmi <= 25.0 = "Normal-ish"
  | bmi <= 30.0 = "Overloaded"
  | otherwise = "wayyyyyy too much"
  where bmi = weight / height ^ 2

-- Recursion fun
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "ahhhhh! Empty List! Empty LIst!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' x y
  | x <= 0 = [] 
  | otherwise = y : (replicate' (x - 1) y)

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:as) (b:bs) = (a,b):zip' as bs

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = 
  let smallerSorted = quickSort [a | a <- xs, a <= x]
      greaterSorted = quickSort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ greaterSorted  
