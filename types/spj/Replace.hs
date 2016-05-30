{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
module Replace where

-- Start uncurried
class Replace a e where
  type Container a e
  replace :: a -> Container a e -> e -> Container a e

instance Replace Int e where
  type Container Int e = [e]
  replace n xs e = let (ys,_:zs) = splitAt n xs
                   in ys ++ e : zs

instance (Replace a e) => Replace (Int,a) e where
  type Container (Int,a) e = [Container a e]
  replace (n,a) xs e = let (ys,x:zs) = splitAt n xs
                       in ys ++ replace a x e : zs
-- Stop uncurried

test = map (map (take 5)) . map (take 5) . take 5
     $ replace (1::Int,(2::Int,3::Int)) (repeat (repeat (repeat ' '))) 'x'

class Curried w1 w2 w3 where
  type Old  w1 w2 w3
  type New  w1 w2 w3
  type Old' w1 w2 w3
  curried :: ((Old  w1 w2 w3 -> New w1 w2 w3 -> Old  w1 w2 w3) ->
              (Old' w1 w2 w3 -> New w1 w2 w3 -> Old' w1 w2 w3)) -> w1 -> w2 -> w3

instance Curried b a b where
  type Old  b a b = a
  type New  b a b = a
  type Old' b a b = b
  curried k = k (\_old new -> new)

instance (Curried w1 w2 w3) => Curried Int w1 (w2 -> w3) where
  type Old  Int w1 (w2 -> w3) = [Old w1 w2 w3]
  type New  Int w1 (w2 -> w3) = New  w1 w2 w3
  type Old' Int w1 (w2 -> w3) = Old' w1 w2 w3
  curried k i = curried (\r -> k (\old new -> let (h,th:tt) = splitAt i old
                                              in h ++ r th new : tt))

replace' :: (Old w1 w2 w3 ~ Old' w1 w2 w3, Curried w1 w2 w3) => w1 -> w2 -> w3
replace' = curried id

test' = map (map (take 5)) . map (take 5) . take 5
      $ (  replace' (1::Int) (2::Int) (3::Int) (repeat (repeat (repeat ' '))) 'x'
        :: [[[Char]]]  )
