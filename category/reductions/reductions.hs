{- 
Recently, I've been thinking about a category of "reductions" over an arbitrary category.

I'll denote R(C) as the category of reductions within C.

The objects of R(C) consist of the arrows in C.

forall A B C D : Obj(C), 
forall f : A -> B, g : C -> D,
there exists (r : f -> g) : Arr(R(C)) if and only if
there exists (encode : A -> C) (decode : D -> B) : Arr(C), where
    f = decode . g . encode

If this is the case, I'll say that f reduces to g via r, or via
(encode, decode)
-}

-- | reducee = decode . reducer . decode
data Reduction a c d b = Reduction {
    reducee :: a -> b,
    reducer :: c -> d,
    encode :: a -> c,
    decode :: d -> b
}

-- | composes two reductions
composeReduction :: Reduction a c d b -> Reduction c e f d -> Reduction a e f b
composeReduction acdb cefd = Reduction {
    reducee = reducee acdb,
    reducer = reducer cefd,
    encode = encode cefd . encode acdb,
    decode = decode acdb . decode cefd 
}
