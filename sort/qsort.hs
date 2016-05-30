qsort :: (Ord o) => [o] -> [o]
qsort [] = []
qsort (x:xs) = (qsort [a | a <- xs, a < x]) ++ [x] ++ (qsort [b | b <- xs, b >= x])

cart :: (Applicative m) => m a -> m b -> m (a, b)

cart as bs = (,) <$> as <*> bs
