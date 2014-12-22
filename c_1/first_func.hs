-- Lists
alphabet = ['a'..'z']
-- This breaks out to the full alphabet

-- lists are lazy by default, so this pulls only the first 20
first20Mults x = take 20 [0,x..]

-- cycle creates a cycle between elements, generating a repeating sequence
-- repeat just repeats the same value over and over

first20MultsComp x = firstN x 20
firstN x n = [i * x | i <- [1..n]]

cartesianProduct x y = [(a,b)|a <- [1..x], b <- [1..y]]
-- zip only creates pairs as long as there is a matching index in both lists
zipped = zip [1..10] ['a'..'i']

rightTriangleWithPerim n = [(a,b,c) | c <- [1..n],
									  b <- [1..c], 
									  a <- [1..b], 
									  -- such that
									  (a^2)+(b^2) == (c^2), 
									  a < b && b < c, 
									  a + b + c == n]

