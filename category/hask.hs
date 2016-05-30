{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (Eq(..), Bool(..), (.), fst, snd, flip, ($))

-- My construction of Hask as a Braided, Monoidal Category based off of
-- "Physics, Topology, Logic and Computation: A Rosetta Stone"

(>>) :: (a -> b) -> (b -> c) -> (a -> c)
f >> g = g . f -- So I can represent arrows as they are in diagrams

id a = a -- The identity morphism for all a

-- A Monoidal Category consists of

-- A category C which in this case is Hask

-- A tensor product functor
-- (,) for objects
(*) :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
f * g = \(a, b) -> (f a, g b)

-- A unit object
unit = ()

-- A natural isomorphism called the associator
a :: ((x, y), z) -> (x, (y, z))
a ((x, y), z) = (x, (y, z))

a_inv :: (x, (y, z)) -> ((x, y), z)
a_inv (x, (y, z)) = ((x, y), z)

aarr :: (((x, y),z) -> ((x', y'), z')) -> (x, (y, z)) -> (x', (y', z'))
aarr f = a . f . a_inv

-- Left and right unitors
l :: ((), x) -> x
l ((), x) = x

r :: (x, ()) -> x
r (x, ()) = x

-- Such that the triangle equation holds for all x, y
triangle_eq :: (Eq x, Eq y) => x -> y -> Bool
triangle_eq x y = (r * id) ((x, ()), y) == (a >> (id * l)) ((x, ()), y)

-- And for all w, x, y, z, the pentagon equation holds 
pentagon_eq :: (Eq w, Eq x, Eq y, Eq z) => w -> x -> y -> z -> Bool
pentagon_eq w x y z = (a >> a) (((w, x), y), z) == ((a * id) >> a >> (id * a)) (((w, x), y), z)

-- The braid
b :: (a, b) -> (b, a)
b (a, b) = (b, a)

b_inv = b

barr :: ((a, b) -> (c, d)) -> (b, a) -> (d, c)
barr f = b . f . b_inv

-- Such that the hexagon equations hold

hexagon_eq1 :: (Eq x, Eq y, Eq z) => x -> y -> z -> Bool
hexagon_eq1 x y z = ((a_inv >> (b * id) >> a >> (id * b) >> a_inv) (x, (y, z)))
                 == (b (x, (y, z)))

hexagon_eq2 :: (Eq x, Eq y, Eq z) => x -> y -> z -> Bool
hexagon_eq2 x y z = ((a >> (id * b) >> a_inv >> (b * id) >> a) ((x, y), z))
                 == (b ((x, y), z))

curry :: (x -> y -> z) -> ((x, y) -> z)
curry f = \(x, y) -> f x y

uncurry :: ((x, y) -> z) -> (x -> y -> z)
uncurry f = \x y -> f (x, y)

eval :: (x, x -> y) -> y
eval (x, f) = f x

eval' = uncurry eval

apply = flip eval'
