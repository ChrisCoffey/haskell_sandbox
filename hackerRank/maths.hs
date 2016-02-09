type Point = (Int, Int)
nextPoint :: Point -> Point -> Point
nextPoint (x,y) (x',y') = (x'+ a ,y' +b) where
    a = x' - x
    b = y' - y

checkPoints = do
    cs <- map (\l -> (map (\x-> read x :: Int) . words) l) . drop 1 <$>  lines <$> getContents
    let inPoints  = map (\[a,b,c,d]-> ((a,b), (c,d))) cs
        points    = map ( uncurry nextPoint) inPoints
        formatted = map (\(x,y)-> (show x)++" "++(show y)) points
    mapM_ putStrLn formatted 

sockmatch = do
    cs <- map (\x-> read x:: Int) . drop 1 <$> lines <$> getContents
    mapM_ (print . (+ 1)) cs

main = print "placeholder"
