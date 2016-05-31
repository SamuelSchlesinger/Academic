{-# LANGUAGE TypeFamilies, NoImplicitPrelude, ScopedTypeVariables #-}


module Delude where

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
    abs f = \x -> abs (f x)
    signum f = \x -> signum (f x)
    fromInteger n = \x -> (fromInteger n)
