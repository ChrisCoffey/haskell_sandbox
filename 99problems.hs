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

