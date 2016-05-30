{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, EmptyDataDecls, ScopedTypeVariables #-}
module Arithm where

-- Start Zero Succ
data Zero
data Succ n
-- Stop Zero Succ

-- Start Nat
class Nat n where
  toInt :: n -> Int
instance Nat Zero where
  toInt _ = 0
instance (Nat n) => Nat (Succ n) where
  toInt _ = 1 + toInt (undefined :: n)
-- Stop Nat

type One   = Succ Zero
type Two   = Succ One
type Three = Succ Two
type Four  = Succ Three
type Five  = Succ Four
type Six   = Succ Five
type Seven = Succ Six
type Eight = Succ Seven
type Nine  = Succ Eight
type Ten   = Succ Nine

-- Start wrappers
newtype Pointer n = MkPointer Int
newtype Offset  n = MkOffset  Int
-- Stop wrappers

-- Start multiple
multiple :: forall n. (Nat n) => Int -> Offset n
multiple i = MkOffset (i * toInt (undefined :: n))
-- Stop multiple

-- Start GCD
class (Nat d, Nat m, Nat n) => HasGCD d m n where
  type GCD d m n
instance (Nat d) => HasGCD d Zero Zero where
  type GCD d Zero Zero = d
instance (Nat d, Nat m, Nat n) => HasGCD d (Succ m) (Succ n) where
  type GCD d (Succ m) (Succ n) = GCD (Succ d) m n
instance (Nat m) => HasGCD Zero (Succ m) Zero where
  type GCD Zero (Succ m) Zero = Succ m
instance (Nat d, Nat m) => HasGCD (Succ d) (Succ m) Zero where
  type GCD (Succ d) (Succ m) Zero = GCD (Succ Zero) d m
instance (Nat n) => HasGCD Zero Zero (Succ n) where
  type GCD Zero Zero (Succ n) = Succ n
instance (Nat d, Nat n) => HasGCD (Succ d) Zero (Succ n) where
  type GCD (Succ d) Zero (Succ n) = GCD (Succ Zero) d n
-- Stop GCD

-- Start add
add :: Pointer m -> Offset n -> Pointer (GCD Zero m n)
add (MkPointer x) (MkOffset y) = MkPointer (x + y)
-- Stop add

fetch32 :: (GCD Zero n Four ~ Four) => Pointer n -> IO ()
fetch32 = undefined

-- General addition
-- Start plus
type family Plus m n
type instance Plus Zero n = n
type instance Plus (Succ m) n = Succ (Plus m n)

plus :: m -> n -> Plus m n
plus = undefined

tplus = plus (undefined::Two) (undefined::Three)
-- Stop plus

tplus' x = if True then plus x (undefined::One) else tplus
