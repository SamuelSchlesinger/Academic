----------------------------------------------------------
-- | Permutation generator
----------------------------------------------------------
filter1 :: (Eq a) => (a -> Bool) -> [a] -> [a]
filter1 p (a:as) | p a = a : filter1 p as
                 | otherwise = as

perms :: (Eq a) => [a] -> [[a]]
perms []  = return []
perms [x] = return [x]
perms xs  = do
  x <- xs
  xs' <- perms $ filter1 (/= x) xs
  return (x : xs')
----------------------------------------------------------
-- | Cartesian product of applicatives 
----------------------------------------------------------
cart :: (Applicative a) => a x -> a y -> a (x, y)
cart xs ys = (,) <$> xs <*> ys
----------------------------------------------------------
-- | Some graph functions
----------------------------------------------------------
type Graph v = ([v], v -> v -> Bool)

transitive, reflexive, symmetric :: Graph v -> Bool

transitive (vs, e) = foldl (&&) True [e a c | a <- vs, b <- vs, e a b, c <- vs, e b c]
reflexive (vs, e) = foldl (&&) True [e a a | a <- vs]
symmetric (vs, e) = foldl (&&) True [e a b == e b a | a <- vs, b <- vs]


