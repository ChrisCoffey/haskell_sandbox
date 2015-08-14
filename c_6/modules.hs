import Data.List
import Data.Char
import qualified Data.Map as Map

ws = words "hey these are words where hey is  thing words can say about where they are"

groupedStrWords str = group . sort $ words str

wsCounts str = map (\x -> (head x, length x)) $ groupedStrWords str

isIn :: (Eq a) => [a] -> [a] -> Bool
a `isIn` as = any (a `isPrefixOf`) $ tails as



encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

decode :: Int -> String -> String
decode offset msg = encode (negate offset) msg

-- foldl' is  a strict foldl, and behaves just like scala's default foldl. Very good to know

--digitSum :: Int -> Int
--digitSum x = sum . map digitToInt . show

--firstDigitSummingTo :: Int -> Maybe Int
--firstDigitSummingTo x = find (\y -> digitSum x == y) [1..]

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs)
	| key == k = Just v
	| otherwise = findKey key xs

findKeyFold key xs = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing xs

