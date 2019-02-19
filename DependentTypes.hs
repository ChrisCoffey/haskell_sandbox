{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-} -- For type-level functions
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE FlexibleInstances #-} -- For HList instances
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DependentTypes where


--
-- Sized Vectors
--

data Nat = Zero | Succ Nat
    deriving Show

data Vector (n :: Nat) (a :: *) where
    VNil :: Vector Zero a
    VCons :: a -> Vector n a -> Vector ('Succ n) a

instance Show a => Show (Vector n a) where
    show VNil = "<>"
    show (VCons a as) = show a ++ "<:>" ++ show as

instance Eq a => Eq (Vector n a) where
    VNil == VNil = True
    (a `VCons` as) == (b `VCons` bs) = a == b && as == bs
    _ == _ = False

add :: Nat -> Nat -> Nat
add n Zero = n -- shortcut
add Zero n = n
add (Succ n) m = add n (Succ m)

fromInt :: Int -> Nat
fromInt n 
    | n == 0 = Zero
    | otherwise = Succ $ fromInt (n - 1)

toInt :: Nat -> Int
toInt Zero = 0
toInt (Succ n) = 1 + (toInt n)

type family n :+ m where
    'Zero :+ n = n
    'Succ n :+ m = 'Succ (n :+ m)

type family n :* m where
    'Zero :* m = 'Zero
    n :* 'Zero = 'Zero
    'Succ 'Zero :* m = m
    'Succ n :* m = n :* (m :+ m)

append :: Vector n a -> Vector m a -> Vector (n :+ m) a
append VNil as = as
append (VCons a as) bs = VCons a (append as bs)

toList :: Vector n a -> [a]
toList VNil = []
toList (VCons a as) = a : toList as
{-
fromList :: [a] -> Vector n a
fromList xs = let
    n = fromInt $ length xs
    in go n xs
    where
    go :: Nat -> [a] -> Vector n a
    go Zero _ = VNil
    go (Succ n) (x:xs) = append (x `VCons` VNil) (go n xs)
-}

vmap :: (a -> b) -> Vector n a -> Vector n b
vmap _ VNil = VNil
vmap f (a `VCons` as) = f a `VCons` (vmap f as)

vinit :: Vector ('Succ n) a -> Vector n a
vinit (a `VCons` as) = case as of
    VNil -> VNil
    _ `VCons` _ -> a `VCons` vinit as

vlast :: Vector n a -> a
vlast (a `VCons` as) = 
    case as of
        VNil -> a
        _ `VCons` _ -> vlast as

vuncons :: Vector ('Succ n) a -> (a, Vector n a)
vuncons (a `VCons` as) = (a, as)

zipWithSame :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
zipWithSame _ VNil _ = VNil
zipWithSame f (a `VCons` as) (b `VCons` bs) = f a b `VCons` zipWithSame f as bs

type family Min n m where
    Min 'Zero m = 'Zero
    Min n 'Zero = 'Zero
    Min ('Succ n) ('Succ m) = 'Succ (Min n m)

vZipWith :: (a -> b -> c) -> Vector n a -> Vector m b -> Vector (Min n m) c
vZipWith _ VNil VNil = VNil
vZipWith _ VNil (_ `VCons` _) = VNil
vZipWith _ (_ `VCons` _) VNil = VNil
vZipWith f (a `VCons` as) (b `VCons` bs) = f a b `VCons` vZipWith f as bs

vfoldr :: (a -> b -> b) -> b -> Vector n a -> b
vfoldr _ seed VNil = seed 
vfoldr f seed (a `VCons` as) = f a (vfoldr f seed as)

--
-- HLists
--

data HList xs where
    HNil :: HList '[]
    (:::) :: a -> HList as -> HList (a ': as)

infixr 6 :::
 
instance Show (HList '[]) where
    show HNil = "'[]"

instance (Show (HList rest), Show a) => Show (HList (a ': rest)) where
    show (a:::rest) = show a ++ " ::: " ++ show rest
   
--
-- Extensible Records
--

newtype s >> a = Named a

-- This is an hlist with named elements rather than just ordinal elements
data HRec xs where
    HEmpty :: HRec '[]
    HCons :: (s >> a) -> HRec xs -> HRec (s >> a ': xs)

instance Show (HRec '[]) where
    show HEmpty = "HEmpty"

instance (Show a, KnownSymbol s) => Show (HRec (s >> a ': xs)) where
    show (HCons (Named a) rest) = 
        let val = show a
            key = symbolVal (undefined :: x s)
