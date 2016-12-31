{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

module TypeGenerics where

data Nat = Zero | Succ Nat

data Vec (a :: *) (n :: Nat) where
    VNil :: Vec a Zero
    VCons :: a -> Vec a n -> Vec a (Succ n)
infixr 5 `VCons`

deriving instance Show a => Show (Vec a n)

type Three = Succ (Succ (Succ Zero))

v3 :: Vec Int Three
v3 = 7 `VCons` 10 `VCons` 18 `VCons` VNil

vHead :: Vec a (Succ n) -> a
vHead (VCons x xs) = x

vTail :: Vec a (Succ n) -> Vec a n
vTail (VCons x xs) = xs

vMap :: (a -> b) -> Vec a n -> Vec b n
vMap _ VNil = VNil
vMap f (VCons x xs) = f x `VCons` (f `vMap` xs)
