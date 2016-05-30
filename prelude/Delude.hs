{-# LANGUAGE TypeFamilies, NoImplicitPrelude, ScopedTypeVariables #-}


module Delude where

import Prelude hiding ((||), (&&), (^), iff, implies, not)
import qualified Data.IntMap as IntMap

-- | A more general logical interface named after the
-- | great man George Boole
class GeorgeLike b where
    (||)     :: b -> b -> b
    (&&)     :: b -> b -> b
    (^)      :: b -> b -> b
    iff      :: b -> b -> b
    implies  :: b -> b -> b
    not      :: b -> b

instance GeorgeLike Bool where
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

instance (GeorgeLike b) => GeorgeLike (x -> b) where
    f || g = \x -> (f x) || (g x)
    f && g = \x -> (f x) && (g x)
    f ^  g = \x -> (f x) ^  (g x)
    f `iff` g = \x -> (f x) `iff` (g x)
    f `implies` g = \x -> (f x) `implies` (g x)
    not f = \x -> not (f x)


