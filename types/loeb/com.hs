{-# LANGUAGE NoImplicitPrelude #-}

import Delude 
import Control.Comonad

data S a = S a (S a)

instance (Show a) => Show (S a) where show (S a s) = show a ++ " " ++ (show s)

instance Functor S where fmap f (S a s) = S (f a) (fmap f s)

instance Comonad S where
    extract (S a _) = a
    duplicate (S a s) = S (S a s) (duplicate s)

constS :: a -> S a
constS a = S a (constS a)
