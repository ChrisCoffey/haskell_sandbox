module Lib where

-- Continuations are simply functions that take additional functions rather than return a value. The following is a CPS version of `if/else`
newtype CPS a r = CPS {runCPS :: ((a -> r) -> r)}

add' :: Num a =>
    a
    -> a
    -> CPS a r
add' a b = CPS $ \next -> next (a+b)

square' :: Num a =>
    a
    -> CPS a r
square' a = CPS $ \next -> next (a*a)

pythagoras' :: Num a =>
    a
    -> a
    -> CPS a r
pythagoras' a b = CPS $ \terminator ->
    runCPS (square' a) $ \a2 ->
    runCPS (square' b) $ \b2 ->
    runCPS (add' a2 b2) $ terminator

-- This doesn't add much beyond confusion

plusTwenty :: Num a =>
    a
    -> CPS a res
plusTwenty x = CPS $ \next -> next $ x+20

terniary ::
    Bool
    -> CPS a r
    -> CPS a r
    -> CPS a r
terniary cond yes no
    | cond = yes
    | otherwise = no

example :: (Num a, Ord a) =>
    a
    -> CPS a r
example n =
    terniary (n < 50)
             (pythagoras' 19 n)
             (plusTwenty n)

