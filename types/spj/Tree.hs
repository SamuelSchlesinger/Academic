module Tree where

-- Start Tree
data Tree a = Tree { zero :: a, pos :: Tree a, neg :: Tree a }
-- Stop Tree

constTree :: a -> Tree a
constTree x = t where t = Tree x t t

toTree :: (Integral a) => (a -> w) -> Tree w
toTree f = Tree (f 0) (toTree p) (toTree n)
  where p i | i < 0 = f (i * (-2))
        p i         = f (i * 2 + 1)
        n i | i > 0 = f (i * (-2))
        n i         = f (i * 2 - 1)

fromTree :: (Integral a) => Tree w -> (a -> w)
fromTree (Tree zero pos neg) i = case (compare i 0, quotRem i 2) of
  (GT, (j,0)) -> fromTree pos (-j)
  (GT, (j,_)) -> fromTree pos j
  (EQ, _    ) -> zero
  (LT, (j,0)) -> fromTree neg (-j)
  (LT, (j,_)) -> fromTree neg j
