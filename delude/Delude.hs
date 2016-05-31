{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}

module Delude
  (
    Boolish(..)
  , module Prelude
  , liftf1, liftf2, lambda
  , Enumerable(..)
  , Sat(..)
  ) where

import Prelude hiding ((||), (&&), (^), iff, implies, not)

-- | Boolish things are things which you can do boolean operations on.
class Boolish b where
    (||), (&&), (^), iff, implies :: b -> b -> b
    not      :: b -> b
    true, false :: b

-- | Bool itself is a Boolish thing.
instance Boolish Bool where
    False || False = False
    _     || _     = True
    True  && True  = True
    _     && _     = False
    False ^  True  = True
    True  ^ False  = True
    _     ^ _      = False
    True  `iff` True = True
    False `iff` False = True
    _     `iff` _     = False
    True `implies` False = False
    _ `implies` _ = True
    not True = False
    not False = True
    true = True
    false = False

lambda :: b -> (a -> b)
lambda b = \a -> b

liftf1 :: (b -> b) -> (a -> b) -> (a -> b)
liftf1 op f = \x -> op (f x)

liftf2 :: (b -> b -> b) -> (a -> b) -> (a -> b) -> (a -> b)
liftf2 op f g = \x -> (f x) `op` (g x)

-- | Functions which return Boolish things are also rather Boolish,
-- | as you can just lift the functions of the Boolish below up a level
-- | of lambda abstraction.
instance (Boolish b) => Boolish (x -> b) where
    (||) = liftf2 (||)
    (&&) = liftf2 (&&)
    (^) = liftf2 (^)
    iff = liftf2 iff
    implies = liftf2 implies
    not = liftf1 not
    true = lambda true
    false = lambda false

-- | The same thing that is done to Boolish things, this lifting
-- | of abstractions, can be done for Num instances.
instance (Num n) => Num (a -> n) where
    (+) = liftf2 (+)
    (-) = liftf2 (-)
    (*) = liftf2 (*)
    negate = liftf1 negate
    abs = liftf1 abs
    signum = liftf1 signum
    fromInteger n = lambda (fromInteger n)

-- | The same applies for Fractional things as Boolish and Num.
instance (Fractional f) => Fractional (a -> f) where
    (/) = liftf2 (/)
    recip = liftf1 recip
    fromRational n = lambda (fromRational n)

-- | Finally, I've lifted the Floating interface.
instance (Floating f) => Floating (a -> f) where
    pi = \x -> pi
    exp = liftf1 exp
    log = liftf1 log
    sqrt = liftf1 sqrt
    (**) = liftf2 (**)
    logBase = liftf2 logBase
    sin = liftf1 sin
    cos = liftf1 cos
    tan = liftf1 tan
    asin = liftf1 asin
    acos = liftf1 acos
    atan = liftf1 atan
    sinh = liftf1 sinh
    cosh = liftf1 cosh
    tanh = liftf1 tanh
    asinh = liftf1 asinh
    acosh = liftf1 acosh
    atanh = liftf1 atanh

-- | A class which supplies you with a (possibly infinite) enumeration of all of the types which instantiate it.
class Enumerable e where enumeration :: [e]

-- | Bounded e, Enum e gives us a natural way to enumerate e, where enumeration = [minBound..maxBound]
instance (Bounded e, Enum e) => Enumerable e where enumeration = [minBound..maxBound]

-- | Gives the user a function which will return whether or not the construction is "satisfiable".
class Sat s where sat :: s -> Bool

-- | True is satisfiable, False is not.
instance Sat Bool where sat = id

-- | A function from some enumerable set to some s with sat defined on it is defined to be whether
-- | any members of the enumeration can satisfy the produced object. This is incredibly inefficient
-- | and should not be used on large spaces if you expect it to take a long time to find a solution.
instance (Enumerable e, Sat s) => Sat (e -> s) where sat f = or (map (sat . f) (enumeration :: [e]))
