import Control.Monad

computeNth :: Integer -> Integer -> Integer -> Integer
computeNth a b 1 = a
computeNth a b n = computeNth b ((b^2) + a) (n - 1)

fibModified :: IO ()
fibModified = do
    (a:b:n:[]) <- liftM (map (\x-> read x :: Integer) . words) getContents
    print $ computeNth a b n

main = fibModified
