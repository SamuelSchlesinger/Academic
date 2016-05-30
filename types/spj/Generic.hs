{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Generic where
import Data.Map (Map)
import qualified Data.Map as Map

-- Start Salary
newtype Salary = Salary Integer
-- Stop Salary
  deriving (Eq, Ord, Show)

-- Start Apply
class Apply f a where
  type Result f a
  apply :: f -> a -> Result f a
-- Stop Apply

-- Start HideSal IncSal
data HideSal = HideSal
instance Apply HideSal Salary where
  type Result HideSal Salary = ()
  apply HideSal _ = ()
instance Apply HideSal Char where
  type Result HideSal Char = Char
  apply HideSal x = x

data IncSal = IncSal
instance Apply IncSal Salary where
  type Result IncSal Salary = Salary
  apply IncSal (Salary x) = Salary (x + 1)
instance Apply IncSal Char where
  type Result IncSal Char = Char
  apply IncSal x = x
-- Stop HideSal IncSal

-- Start containers
instance (Apply f a, Apply f b) => Apply f (a,b) where
  type Result f (a,b) = (Result f a, Result f b)
  apply f (x,y) = (apply f x, apply f y)

instance (Apply f a) => Apply f [a] where
  type Result f [a] = [Result f a]
  apply f = map (apply f)
-- Stop containers

instance (Apply HideSal a, Ord (Result HideSal a))
      => Apply HideSal (Map salary [a]) where
  type Result HideSal (Map salary [a])
     = Map (Result HideSal a) Integer
  apply HideSal m = Map.fromListWith (+)
    [ (apply HideSal x, 1) | x <- concat (Map.elems m) ]

instance (Apply IncSal a) => Apply IncSal (Map Salary a) where
  type Result IncSal (Map Salary a)
     = Map Salary (Result IncSal a)
  apply IncSal = Map.map (apply IncSal)
               . Map.mapKeysMonotonic (apply IncSal)

test = apply HideSal ("Alice", Salary 42)
