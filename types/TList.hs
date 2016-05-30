{-# LANGUAGE ScopedTypeVariables #-}

data TwoList a b = a :/: (TwoList b a) | Nil

instance (Show a, Show b) => Show (TwoList a b) where
    show (a :/: Nil) = show a
    show (a :/: ts) = show a ++ " " ++ show ts
    show Nil = "" 

tuplify :: TwoList a b -> [(a, b)]

tuplify (a :/: (b :/: rest)) = (a, b) : (tuplify rest)

twozip :: [a] -> [b] -> TwoList a b
twozip [] _ = Nil
twozip (a:as) bs = a :/: (twozip bs as)
