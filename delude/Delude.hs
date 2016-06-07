{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}

module Delude
  (
    Boolish(..)
  , module Prelude
  , module Control.Monad
  , module Control.Applicative
  , module Data.Functor
  , module Data.Foldable
  , liftf1, liftf2
  , Enumerable(..)
  , Stream(..)
  ) where

import Data.Functor
import Control.Applicative
import Control.Monad
import Control.Comonad
import Data.Foldable
import Prelude hiding ((||), (&&), (^), iff, implies, not, elem, iterate)

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

---------------------------------------------------------------------
 
liftf1 :: (b -> b) -> (a -> b) -> (a -> b)
liftf1 = (.)

liftf2 :: (b -> b -> b) -> (a -> b) -> (a -> b) -> (a -> b)
liftf2 op f g x = f x `op` g x

{-class Mapf m where mapf :: m (a -> b) -> a -> m b

instance Mapf [] where 
    mapf [] a = []
    mapf (f:fs) a = f a : (mapf fs a)

liftfN :: (b -> b -> b) -> [a -> b] -> a -> b
liftfN op (f:g:fs) = liftfN op ((liftf2 op f g):fs)

------------------------------------------------------------------------ -}

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
    true = const true
    false = const false

-- | The same thing that is done to Boolish things, this lifting
-- | of abstractions, can be done for Num instances.
instance (Num n) => Num (a -> n) where
    (+) = liftf2 (+)
    (-) = liftf2 (-)
    (*) = liftf2 (*)
    negate = liftf1 negate
    abs = liftf1 abs
    signum = liftf1 signum
    fromInteger n = const (fromInteger n)

-- | The same applies for Fractional things as Boolish and Num.
instance (Fractional f) => Fractional (a -> f) where
    (/) = liftf2 (/)
    recip = liftf1 recip
    fromRational n = const (fromRational n)

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

-- | Composable things
class Arr arr where
    (.<) :: arr b c -> arr a b -> arr a c
    (>.) :: arr a b -> arr b c -> arr a c
    f >. g = g .< f
    f .< g = g >. f

instance Arr (->) where (.<) = (.)

-- | A class which supplies you with a (possibly infinite) enumeration of all of the types which instantiate it.
class Enumerable e where enumeration :: [e]

class Iterable s where iterate :: (i -> i) -> i -> s i

instance Iterable [] where iterate f x = x : (iterate f (f x))

instance Iterable Stream where iterate f x = Cons x (iterate f (f x))

instance (Num a, Num b) => Num (a, b) where
    (a, b) + (c, d) = (a + c, b + d)
    (a, b) * (c, d) = (a * c, b * d)
    (a, b) - (c, d) = (a - c, b - d)
    negate (a, b) = (negate a, negate b)
    abs (a, b) = (abs a, abs b)
    signum (a, b) = (signum a, signum b)
    fromInteger n = (fromInteger n, fromInteger n)

instance (Fractional a, Floating b) => Fractional (a, b) where
    (a, b) / (c, d) = (a / c, b / d)
    recip (a, b) = (recip a, recip b)
    fromRational n = (fromRational n, fromRational n)

instance (Floating a, Floating b) => Floating (a, b) where
    pi = (pi, pi)
    exp (a, b) = (exp a, exp b)
    log (a, b) = (log a, log b)
    sqrt (a, b) = (sqrt a, sqrt b)
    (a, b) ** (c, d) = (a ** c, b ** d)
    logBase (a, b) (c, d) = (logBase a c, logBase b d)
    sin (a, b) = (sin a, sin b)
    cos (a, b) = (cos a, cos b)
    tan (a, b) = (tan a, tan b)
    asin (a, b) = (asin a, asin b)
    acos (a, b) = (acos a, acos b)
    atan (a, b) = (atan a, atan b)
    sinh (a, b) = (sinh a, sinh b)
    cosh (a, b) = (cosh a, cosh b) 
    tanh (a, b) = (tanh a, tanh b)
    asinh (a, b) = (asinh a, asinh b)
    acosh (a, b) = (acosh a, acosh b)
    atanh (a, b) = (atanh a, atanh b)

instance (Num a, Num b, Num c) => Num (a, b, c) where
    (a, b, c) + (d, e, f) = (a + d, b + e, c + f)
    (a, b, c) * (d, e, f) = (a * d, b * e, c * f)
    (a, b, c) - (d, e, f) = (a - d, b - e, c - f)
    negate (a, b, c) = (negate a, negate b, negate c)
    abs (a, b, c) = (abs a, abs b, abs c)
    signum (a, b, c) = (signum a, signum b, signum c)
    fromInteger n = (fromInteger n, fromInteger n, fromInteger n)    

instance (Fractional a, Fractional b, Fractional c) => Fractional (a, b, c) where
    (a, b, c) / (e, f, g) = (a / e, b / f, c / g)
    recip (a, b, c) = (recip a, recip b, recip c)
    fromRational n = (fromRational n, fromRational n, fromRational n)

instance (Floating a, Floating b, Floating c) => Floating (a, b, c) where
    pi = (pi, pi, pi)
    exp (a, b, c) = (exp a, exp b, exp c)
    log (a, b, c) = (log a, log b, log c)
    sqrt (a, b, c) = (sqrt a, sqrt b, sqrt c)
    (a, b, e) ** (c, d, f) = (a ** c, b ** d, e ** f)
    logBase (a, b, e) (c, d, f) = (logBase a c, logBase b d, logBase e f)
    sin (a, b, c) = (sin a, sin b, sin c)
    cos (a, b, c) = (cos a, cos b, cos c)
    tan (a, b, c) = (tan a, tan b, tan c)
    asin (a, b, c) = (asin a, asin b, asin c)
    acos (a, b, c) = (acos a, acos b, acos c)
    atan (a, b, c) = (atan a, atan b, atan c)
    sinh (a, b, c) = (sinh a, sinh b, sinh c)
    cosh (a, b, c) = (cosh a, cosh b, cosh c) 
    tanh (a, b, c) = (tanh a, tanh b, tanh c)
    asinh (a, b, c) = (asinh a, asinh b, asinh c)
    acosh (a, b, c) = (acosh a, acosh b, acosh c)
    atanh (a, b, c) = (atanh a, atanh b, atanh c)
