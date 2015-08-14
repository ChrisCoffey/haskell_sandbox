-- Integers change their size based on the compiler. So for a 64 bit compiler, it's 64 bits. 32 bit gets 32 bits...
factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r


-- polymorphic functions are just functions that use type variables.
-- THis gets into type-level programming pretty heavily

-- type class = an interface that defines some behavior. This is not an implementation!
-- | This is haddock documentation. There are lots of things here
