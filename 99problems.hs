import Control.Monad

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
    
