{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

class TShow t where
    tshow :: t -> String

instance TShow Char where tshow _ = "Char"
instance TShow Bool where tshow _ = "Bool"
instance TShow Int where tshow _ = "Int"
instance TShow Integer where tshow _ = "Integer"
instance TShow Ordering where tshow _ = "Ordering"
instance (TShow ((->) a b)) => Show ((->) a b) where show = tshow
instance (TShow a) => TShow (IO a) where tshow _ = "IO " ++ tshow (undefined :: a)
instance (TShow a) => TShow ([a]) where tshow _ = "[" ++ tshow (undefined :: a) ++ "]"

instance (TShow a, TShow b) => TShow (a -> b) where
    tshow _ = "(" ++ tshow (undefined :: a) ++ " -> " 
           ++ tshow (undefined :: b) ++ ")"

