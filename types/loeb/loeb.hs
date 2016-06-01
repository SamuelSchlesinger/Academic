loeb :: Functor a => a (a x -> x) -> a x
loeb x = fmap (\a -> a (loeb x)) x
