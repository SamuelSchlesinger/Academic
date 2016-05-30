{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeFamilies #-}
module Coercion where

-- Start basic
class Add a b where
  type SumTy a b
  add :: a -> b -> SumTy a b

instance Add Integer Double where
  type SumTy Integer Double = Double
  add x y = fromIntegral x + y

instance Add Double Integer where
  type SumTy Double Integer = Double
  add x y = x + fromIntegral y

instance (Num a) => Add a a where
  type SumTy a a = a
  add x y = x + y
-- End basic

-- Start recursive
instance (Add Integer a) => Add Integer [a] where
  type SumTy Integer [a] = [SumTy Integer a]
  add x y = map (add x) y
-- End recursive

-- A variation for heterogeneous lists
-- Start Cons
class Cons a b where
  type ResTy a b
  cons :: a -> [b] -> [ResTy a b]

instance Cons Integer Double where
  type ResTy Integer Double = Double
  cons x ys = fromIntegral x : ys

-- ...
-- End Cons


instance Cons Double Integer where
  type ResTy Double Integer = Double
  cons x ys = x : map fromIntegral ys

instance Cons a a where
  type ResTy a a = a
  cons x ys = x : ys

tcons = cons three $ cons two $ cons two $ [one]
 where one   = 1::Integer
       two   = 2.0::Double
       three = 3::Integer
