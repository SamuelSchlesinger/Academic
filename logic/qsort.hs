sort :: (Ord o) => [o] -> [o]
sort [] = []
sort (x:xs) = (sort [a | a <- xs, a < x]) ++ [x] ++ (sort [a | a <- xs, a >= x])

cart :: [a] -> [b] -> [(a, b)]
cart as bs = [(a, b) | a <- as, b <- bs]

