import Control.Monad
import qualified Data.Map               as M
import qualified Data.List              as L

main = do
    print "Im main"

data YN = YES | NO deriving (Show)

zig :: [a] -> [(a,a)]
zig ls = map snd . filter (even . fst) . zip [0 :: Int ..] . zip ls $ tail ls

--string substring search
isSubstring :: (String, String) -> Bool
isSubstring (str,search@(h:hs)) = f str search || f (reverse str) (reverse search) where
    f _ [] = True
    f [] (x:rest) = False
    f (a:rest) (b:rest')
        | a == b = f rest rest'
        | a == h = f rest hs
        | otherwise = f rest search

substringCheck :: IO ()
substringCheck =  do
    getContents >>= mapM_ print . map (\b-> if b then YES else NO) . map isSubstring . zig . drop 1 . lines

--binary search tree preorder traversal
data BinaryTree a = EmptyTree | TreeNode (BinaryTree a) a (BinaryTree a) deriving (Show)

btAdd :: (Ord a) => a -> BinaryTree a -> BinaryTree a
btAdd a EmptyTree = TreeNode EmptyTree a EmptyTree
btAdd a (TreeNode l v r)
    | a > v = TreeNode l v (btAdd a r)
    | a < v = TreeNode (btAdd a l) v r

bTfromList :: (Ord a) => [a] -> BinaryTree a
bTfromList ls = foldl (\t i-> btAdd i t) EmptyTree ls

preorderTraversal :: BinaryTree a -> [a]
preorderTraversal EmptyTree = []
preorderTraversal (TreeNode l v r) =
    [v] ++ (preorderTraversal l) ++ (preorderTraversal r)

preorderCheck :: IO ()
preorderCheck = do
    input <- liftM (map  ( map (\a-> read a :: Int) . words . snd) . filter (odd . fst) . zip [0:: Int ..] . drop 1 . lines) $ getContents
    mapM_ print $  map(\b-> if b then YES else NO) . map (\ls -> ls == (preorderTraversal . bTfromList $ ls)) $ input


-- fibonacci numbers 
fibs = 0:1:(fr 0 1) where
    fr n m = (n+m):(fr m (n+m))

nthFibMod n = 
   (`mod` 100000007) . head . drop (n) $ fibs

fibCheck :: IO ()
fibCheck = do
    getContents >>= mapM_ print . map (nthFibMod . read) . drop 1 . lines

-- pentagonal numbers
generalizedPent n = (3 * (n ^ 2) - n) `div` 2

pents = map (p) [0..] where
    p n = generalizedPent n

pentCheck :: IO ()
pentCheck = do
    getContents >>= mapM_ print . map (generalizedPent. read) . drop 1 . lines

-- misisng numbers
aggLists :: [Int] -> [Int] -> [Int]
aggLists l r = let
    mappify = foldl (\acc x-> M.insertWith (+) x 1 acc) (M.fromList[(0,0)])
    ma = mappify l
    mb = mappify r
    diffs = M.filterWithKey (\b _ -> (M.lookup b ma) /= (M.lookup b mb )) mb
    in L.sort . map fst . M.toList $ diffs

sndNFourth (a:b:c:d:[])= b:d:[]
    

missingCheck :: IO ()
missingCheck = do
    cs <-  getContents
    let lns = lines cs
        (h:t:[]) = sndNFourth lns
        hs = map (\x -> read x ::Int) . words $ h
        ts = map (\x -> read x ::Int) . words $ t
        agg = aggLists hs ts
        -- There must be a better way...
        fmt = reverse . tail . reverse .foldr (\n str-> (show n)++(' ':str)) "" $ agg
    putStr fmt
