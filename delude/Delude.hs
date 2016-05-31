{-# LANGUAGE NoImplicitPrelude #-}

module Delude
  (
    Boolish(..)
  , module Prelude
  ) where

import Prelude hiding ((||), (&&), (^), iff, implies, not)

class Boolish b where
    (||), (&&), (^), iff, implies :: b -> b -> b
    not      :: b -> b

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

instance (Boolish b) => Boolish (x -> b) where
    f || g = \x -> (f x) || (g x)
    f && g = \x -> (f x) && (g x)
    f ^  g = \x -> (f x) ^  (g x)
    f `iff` g = \x -> (f x) `iff` (g x)
    f `implies` g = \x -> (f x) `implies` (g x)
    not f = \x -> not (f x)


instance (Num n) => Num (a -> n) where
    a + b = \x -> a x + b x
    a - b = \x -> a x - b x
    a * b = \x -> a x * b x
    negate f = \x -> negate (f x)
    abs = \x -> abs x
    signum f = \x -> signum (f x)
    fromInteger n = \x -> (fromInteger n)

instance (Fractional f) => Fractional (a -> f) where
    f / g = \x -> (f x) / (g x)
    recip f = \x -> (fromRational 1) / (f x)
    fromRational n = \x -> (fromRational n)

instance (Floating f) => Floating (a -> f) where
    pi = \x -> pi
    exp f = \x -> exp (f x)
    log f = \x -> log (f x)
    sqrt f = \x -> sqrt (f x)
    f ** g = \x -> (f x) ** (g x)
    logBase f g = \x -> logBase (f x) (g x)
    sin f = \x -> sin (f x)
    cos f = \x -> cos (f x)
    tan f = \x -> tan (f x)
    asin f = \x -> asin (f x)
    acos f = \x -> acos (f x)
    atan f = \x -> atan (f x)
    sinh f = \x -> sinh (f x)
    cosh f = \x -> cosh (f x)
    tanh f = \x -> tanh (f x)
    asinh f = \x -> asinh (f x)
    acosh f = \x -> acosh (f x)
    atanh f = \x -> atanh (f x) 
