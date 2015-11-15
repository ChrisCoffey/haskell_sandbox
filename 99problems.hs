problem1 ls = (head . reverse) ls

problem2 ls = (head . tail . reverse) ls

problem3 ls n = head (drop (n - 1) ls)

problem4 ls = foldl (\acc e -> acc + 1) 0 ls

problem5 ls = foldl (\acc e -> e : acc) [] ls

problem6 :: (Eq a) => [a] -> Bool
problem6 ls = all (\(x,y) -> x == y )(zip ls (reverse ls)) 

data NestedList a = Elem a | List [NestedList a]
problem7 :: NestedList a -> [a]
problem7 ls = 
