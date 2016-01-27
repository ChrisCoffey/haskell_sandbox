import Control.Monad

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
data BinaryTree a = EmptyTree | TreeNode BinaryTree a BinaryTree deriving (Show)

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




