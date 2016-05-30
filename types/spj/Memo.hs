{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances, GeneralizedNewtypeDeriving #-}
-- The last two extensions are needed for the Fix example, memoizing
-- data types defined as fix-points of functors

module Memo where
import Tree

-- Start class
class Memo a where
  data Table a :: * -> *
  toTable :: (a -> w) -> Table a w
  fromTable :: Table a w -> (a -> w)
-- Stop class

-- Start Bool
instance Memo Bool where
  data Table Bool w = TBool w w
  toTable f = TBool (f True) (f False)
  fromTable (TBool x y) b = if b then x else y
-- Stop Bool

instance Memo () where
  newtype Table () w = TUnit w
  toTable f = TUnit (f ())
  fromTable (TUnit x) () = x

-- Start product
instance (Memo a, Memo b) => Memo (a,b) where
  newtype Table (a,b) w = TProduct (Table a (Table b w))
  toTable f = TProduct (toTable (\x -> toTable (\y -> f (x,y))))
  fromTable (TProduct t) (x,y) = fromTable (fromTable t x) y
-- Stop product

-- Start sum
instance (Memo a, Memo b) => Memo (Either a b) where
  data Table (Either a b) w = TSum (Table a w) (Table b w)
  toTable f = TSum (toTable (f . Left)) (toTable (f . Right))
  fromTable (TSum t _) (Left  v) = fromTable t v
  fromTable (TSum _ t) (Right v) = fromTable t v
-- Stop sum

-- Start list
instance (Memo a) => Memo [a] where
  data Table [a] w = TList w (Table a (Table [a] w))
  toTable f = TList (f []) 
                    (toTable (\x -> toTable (\xs -> f (x:xs))))
  fromTable (TList t _) []     = t
  fromTable (TList _ t) (x:xs) = fromTable (fromTable t x) xs
-- Stop list

-- Start Integer
instance Memo Integer where
  newtype Table Integer w = TInteger { unTInteger :: Tree w }
  toTable = TInteger . toTree
  fromTable = fromTree . unTInteger
-- Stop Integer

instance Memo Char where
  newtype Table Char w = TChar { unTChar :: Tree w }
  toTable = TChar . toTree . (. toEnum)
  fromTable = (. fromEnum) . fromTree . unTChar

data Fix f a = Fix (f (Fix f) a)

instance (Memo (f (Fix f) a)) => Memo (Fix f a) where
  data Table (Fix f a) w = TFix (Table (f (Fix f) a) w)
  toTable fn = TFix (toTable (\x -> fn (Fix x)))
  fromTable (TFix t) = \(Fix x) -> fromTable t x

newtype ListF f a = ListF (Either () (a,f a)) deriving Memo
type List a = Fix ListF a


-- Example for the list of booleans

fbooll :: List Bool -> Integer
fbooll (Fix (ListF (Left ()))) = 0
fbooll (Fix (ListF (Right (h,t)))) = 
    (fromIntegral . fromEnum $ h) + 2 * fbooll t

tbooll = cons True $ cons False $ cons True $ cons True nil
 where
 nil  = Fix (ListF (Left ()))
 cons x f = Fix (ListF (Right (x,f)))

test_fbooll = (fr,gr)
  where fr = fbooll tbooll
	gr = gbooll tbooll
	gbooll = fromTable (toTable fbooll)
