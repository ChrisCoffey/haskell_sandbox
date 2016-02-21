
import Data.Functor
import qualified Data.Map               as M
import qualified Data.List              as L




main = do
    ls <- drop 1 <$> lines <$> getContents
    let 
        (a:b:[]) = ls
        as = map (\s-> read s :: Int) . words $ a
        bs = map (\s-> read s :: Int) . words $ b
        pairs = zip as bs
    print pairs
