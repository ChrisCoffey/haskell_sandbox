module Lib where

-- Continuations are simply functions that take additional functions rather than return a value. The following is a CPS version of `if/else`
newtype CPS r a  = CPS {runCPS :: ((a -> r) -> r)}


add' :: Num a =>
    a
    -> a
    -> CPS r a
add' a b = CPS $ \next -> next (a+b)

square' :: Num a =>
    a
    -> CPS r a
square' a = CPS $ \next -> next (a*a)

pythagoras' :: Num a =>
    a
    -> a
    -> CPS r a
pythagoras' a b = CPS $ \terminator ->
    runCPS (square' a) $ \a2 ->
    runCPS (square' b) $ \b2 ->
    runCPS (add' a2 b2) $ terminator

-- This doesn't add much beyond confusion

plusTwenty :: Num a =>
    a
    -> CPS res a
plusTwenty x = CPS $ \next -> next $ x+20

terniary ::
    Bool
    -> CPS r a
    -> CPS r a
    -> CPS r a
terniary cond yes no
    | cond = yes
    | otherwise = no

example :: (Num a, Ord a) =>
    a
    -> CPS r a
example n =
    terniary (n < 50)
             (pythagoras' 19 n)
             (plusTwenty n)


--
-- try {
--    a
-- } catch {
--    b
-- }
-- c
--
try ::
    (CPS r a -> CPS r a -> CPS r a) -- ^ try body
    -> (CPS r a -> CPS r a -> CPS r a) -- ^ catch body
    -> CPS r a -- ^ outer normal
    -> CPS r a -- ^ outer error
    -> CPS r a
try tBody cBody outerSucc outerErr = CPS $ \next ->
    runCPS (tBody outerSucc $ cBody outerSucc outerErr) $ next


-- Given two CPS, call the second one
throw ::
    CPS r a -- ^ normal path. This is ignored
    -> CPS r a -- ^ Error path
    -> CPS r a
throw = flip const

tryCatchExample :: (Num a, Ord a) =>
    a
    -> CPS r a
tryCatchExample n =
    try
        (\ happy sad -> terniary (n < 10)
                  (throw happy $ CPS (error "boo"))
                  (sad)
        ) -- try body
        (\happy sad -> terniary (n > 50)
                  happy
                  sad
        ) -- catch body
        (plusTwenty n) -- outer next step
        topLevelError -- outer error handler
    where
        topLevelError = CPS $ error "Hit an error!"


-- Build up a function that returns ((b -> r) -> r). Do this by combining the two CPS calls together.

chainCPS ::
    ((a -> r) -> r)
    -> (a -> ((b -> r) -> r))
    -> ((b -> r) -> r)
chainCPS cont next =
    \a -> cont $ \x -> next x $ a
    -- a ^ above is a (b -> r), while x is an 'a'. This is clear because `cont` takes an `a -> r`, so the
    -- lambda passed to it has an `a` as its first argument. Similarly, next first takes an `a`, then a
    -- continuation. That continuation is the argument `a`. So the entire function returns an `r` after all
    -- the composition is completed.

instance Functor (CPS r) where
    fmap f (CPS cont) =
        CPS $ \next -> cont $ \a -> next $ f a

instance Applicative (CPS r) where
    pure a = CPS $ \next -> next a
    -- f :: (((a -> b) -> r) -> r)
    -- cont :: ((a -> r) -> r)
    -- <*> :: (b -> r) -> r
    (CPS f) <*> (CPS cont) =
        CPS $ \next ->
            f $ \abr -> cont $ next . abr

instance Monad (CPS r) where
    (CPS cont) >>= next =
        CPS $ \b -> cont $ \a -> runCPS (next a) b
