{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}


module Key (Key(..)) where

import qualified TST as T
import qualified RBTree as R

class Key k where
    data Map k :: * -> *
    lookup :: Map k v -> k -> v
    insert :: Map k v -> (k, v) -> Map k v
    empty  :: Map k v

instance Key String where
    data Map String v = T String v
    lookup = T.snag
    insert = T.insert
    empty = T.empty

instance Key Integer where
    data Map Integer v = RBTree Integer v
    lookup = R.get
    insert = R.insert
    empty = R.empty
