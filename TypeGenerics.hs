{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeGenerics where

import GHC.Exts
import Data.Proxy

 --This is an unihabited type, meaning there are no values of type Nat
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

vLength :: Vec a n -> Int
vLength VNil = 0
vLength (VCons _ xs) = 1 + vLength xs

-- SNat is a singleton, meaning there is only a single value for each type
-- This type + class exist to recursively reverse/unwind a Nat
data SNat (n::Nat) where
    SZero :: SNat Zero
    SSuc :: SNatI n => SNat (Succ n)

class SNatI (n :: Nat) where
    sNat :: SNat n

instance SNatI Zero where
    sNat = SZero

instance SNatI n => SNatI (Succ n) where
    sNat = SSuc

-- I'm missing a GHC extension to fully support this. Right now I'm unable
-- to resolve (vRepliate.a ~ go.a)
{-
vreplicate :: SNatI n => a -> Vec a n
vreplicate x = go sNat
    where
        go :: SNat n -> Vec a n
        go SZero = VNil
        go SSuc = x `VCons` go sNat
-}

vApply :: Vec (a -> b) n -> Vec a n -> Vec b n
vApply VNil VNil = VNil
vApply (f `VCons` fs) (x `VCons` xs) = f x `VCons` vApply fs xs

data HList (xs :: [*]) where
    HNil :: HList '[] -- the quote here tells GHC this is the empty type level list
    HCons :: x -> HList xs -> HList (x ': xs)
infixr 5 `HCons`

hG :: HList [Int, Char, Bool]
hG = 7 `HCons` 'a' `HCons` True `HCons` HNil

{- N-ary product
 -
 - 'f' is a function that maps some kind 'k' back to '*',
 - while 'xs' is a list of kind 'k'. This is a generalization of an 
 - 'HList', where 'k' ~ '*'
 -
 - 'f' is an interpretation function mapping 'k' -> '*'
 -}
data NP (f :: k -> *) (xs :: [k]) where
    NNil :: NP f '[]
    (:*) :: f x -> NP f xs -> NP f (x ': xs)
infixr 5 :*

-- Type-level identity funcition. forall a, 'TIdent' a <-> a
newtype TIdent a = TIdent {runTIdent :: a}

-- Proof that 'NP' is a generalization of HList
fromHList :: HList xs -> NP TIdent xs
fromHList HNil = NNil 
fromHList (HCons x xs) = TIdent x :* fromHList xs 

toHList :: NP TIdent xs -> HList xs
toHList NNil = HNil
toHList (x :* xs) = runTIdent x `HCons` toHList xs

-- May be partially applied to some type 'a' then used in NP to bind all elements to the same type
-- in the resulting HList
newtype K a b = K {runK :: a} 

-- Works provided 'f' is polymorphic. This is what's meant by a rank-n type, or n-ary polymprphism
-- Although in this case it is just a rank-2 function
hmap :: (forall x. f x -> g x) -> NP f x -> NP g x
hmap _ NNil = NNil
hmap f (x :* xs) = f x :* hmap f xs

npG :: NP TIdent '[Int, String, Bool]
npG = TIdent 7 :* TIdent "ABC" :* TIdent True :* NNil

{- 'NP' creates a type preserving map (function) if used with 'TIdent' because the type of the 
 - element passed to 'TIdent' is "preserved" when creating the n-ary product
 -
 - 'NP' creates a type unifying map if used with 'K' because 'K' maps any incoming type to a single
 - outgoing type. In this way, an 'NP' of 'K' is isomporphic to a standard 'Vector' (with type-level length).
 -
 - Finally, if 'NP' is 'hmap'ed from 'K' -> 'I' then it becomes a generative map, creating new types
 - from the homogenous 'Vector'
 -
 - Cool stuff!
 -}

-- Create singletons for the list constructors so we can recurse
data SList (xs:: [k]) where
    SNil :: SList '[]
    SCons :: SListI xs => SList (x ': xs)

class SListI (a :: [k]) where
    sList :: SList a

instance SListI '[] where
    sList = SNil

instance SListI xs => SListI (x ': xs) where
    sList = SCons

hpure :: forall f xs. SListI xs => (forall a. f a) -> NP f xs
hpure x = go sList
    where
    go :: forall xs. SList xs -> NP f xs
    go SNil = NNil
    go SCons = x :* go sList
 
{- WIP exercise. I'm not sure if this is possible with singelton types because it requires
 - jumping between the type an value levels. Using a 'K' it seems a bit more plausible, but
 - I'll need some type-level function that compares between the two types on a 'K'
hRemove :: (SListI xs, SListI ys) => K a b -> NP f xs -> NP f ys 
hRemove _ NNil = NNil
hRemove k (x :* xs) = go k x
    where 
    go :: 
-} 

--This somewhat addresses the above concern about modifying (via `apply`) an HList

newtype (f -.-> g) a = Fn {apFn :: f a -> g a}
infixr 1 -.->

hApply :: NP (f -.-> g) xs -> NP f xs -> NP g xs
hApply NNil NNil = NNil
hApply (f :* fs) (x :* xs) = apFn f x :* hApply fs xs

l <&> r = hApply l r
infixl 5 <&>

--Makes sense because I'm using the list kind
ls :: NP [] '[String, Int]
ls = ["foo", "bar", "baz"] :* [1..10] :* NNil

numbers :: NP (K Int) '[String, Int]
numbers = K 2 :* K 5 :* NNil

fn_2 :: (f a -> g a -> h a) -> (f -.-> g -.-> h) a
fn_2 f = Fn (Fn . f ) 

hTake :: (K Int -.-> [] -.-> []) a
hTake = fn_2 (\(K n) xs -> take n xs)

--Proof that application actually produces the correct type
hApplyTest :: NP [] '[String, Int]
hApplyTest = hpure hTake <&> numbers <&> ls


--Getting into type functions, beyond simple data structures

{- This expands out via `:kind!` in ghci to apply `Eq` to each element of the hList -}
type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint where
    All c '[] = ()
    All c (x ': xs) = (c x, All c xs)

hListToString :: All Show xs => HList xs -> String
hListToString HNil = ""
hListToString (x `HCons` xs) = show x ++ hListToString xs

class (f (g x)) => (f `Compose` g) x
instance (f (g x)) => (f `Compose` g) x

npToString :: All (Show `Compose` f) xs => NP f xs -> String
npToString NNil = ""
npToString (x :* xs) = show x ++ npToString xs

deriving instance (All (Compose Show f) xs) => Show (NP f xs)

-- Allows a specific constraint on the 
hcPure :: 
    forall c f xs.  (SListI xs, All c xs) => 
    Proxy c -> 
    (forall a. c a => f a) -> 
    NP f xs
hcPure p x = go sList
    where
    go :: forall xs. All c xs => SList xs -> NP f xs
    go SNil = NNil
    go SCons = x :* go sList
