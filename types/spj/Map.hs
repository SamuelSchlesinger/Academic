{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeFamilies, ScopedTypeVariables #-}
module Map where
import Prelude hiding ( lookup )
import qualified Data.IntMap

-- Start class
class Key k where
  data Map k :: * -> *
  empty  :: Map k v
  lookup :: k -> Map k v -> Maybe v
  -- ...many other methods could be added...
-- Stop class

-- Start instances
instance Key Bool where
  data Map Bool elt = MB (Maybe elt) (Maybe elt)
  empty = MB Nothing Nothing
  lookup False (MB mf _) = mf
  lookup True  (MB _ mt) = mt

instance (Key a, Key b) => Key (Either a b) where
  data Map (Either a b) elt = MS (Map a elt) (Map b elt)
  empty = MS empty empty
  lookup (Left  k) (MS m _) = lookup k m
  lookup (Right k) (MS _ m) = lookup k m

instance (Key a, Key b) => Key (a,b) where
  data Map (a,b) elt = MP (Map a (Map b elt))
  empty = MP empty
  lookup (a,b) (MP m) = case lookup a m of
			Nothing -> Nothing
			Just m' -> lookup b m'
-- Stop instances

-- Start int
instance Key Int where
  newtype Map Int elt = MI (Data.IntMap.IntMap elt)
  empty = MI Data.IntMap.empty
  lookup k (MI m) = Data.IntMap.lookup k m
-- Stop int

instance (Key a) => Key [a] where
  data Map [a] elt = ML (Maybe elt) (Map a (Map [a] elt))
  empty = ML Nothing empty
  lookup [] (ML m0 _) = m0
  lookup (h:t) (ML _ m1) = case lookup h m1 of
			   Nothing -> Nothing
			   Just m' -> lookup t m'
