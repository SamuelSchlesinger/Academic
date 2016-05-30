{-# LANGUAGE GADTs #-}

module RBTree (RBTree(..), empty, depth, get, insert) where

-- Red black tree based on Chris Okasaki's paper:
-- Red-Black Trees in a Functional Setting
-- (https://wiki.rice.edu/confluence/download/attachments/2761212/Okasaki-Red-Black.pdf)

data Color = Red | Black deriving (Show)

data RBTree key val where
     RBNil  :: RBTree key val
     RBNode :: (Ord key) =>  
                Color -> RBTree key val -> (key, val) -> RBTree key val -> RBTree key val

instance (Show key, Show val) => Show (RBTree key val) where
    show RBNil = ""
    show (RBNode _ left (key, value) right) = (show left) ++ " (" ++ (show key) ++ ", " ++ (show value) ++ ") " ++ (show right)

-- Invariant 1: No red node has a red parent
-- Invariant 2: Every path from the root to an empty node contains the
--              same number of black nodes

empty :: RBTree key val
empty = RBNil

depth :: RBTree key val -> Integer
depth RBNil = 1
depth (RBNode _ left _ right) | dleft < dright = dleft + 1
                            | otherwise      = dright + 1 where
    dleft  = depth left
    dright = depth right

get :: RBTree key val -> key -> Maybe val
get RBNil _ = Nothing
get (RBNode _ left (key, val) right) key' 
    | key >  key' = get left  key'
    | key == key' = Just val
    | key < key'  = get right key'

insert :: (Ord key) => RBTree key val -> (key, val) -> RBTree key val
insert tree (key, val) = makeBlack (insert' tree) where
    makeBlack (RBNode _ left (key, val) right) = RBNode Black left (key, val) right
    insert' RBNil = RBNode Red RBNil (key, val) RBNil
    insert' (RBNode col left (key', val') right) 
        | key <  key' = balance col (insert' left) (key', val') right
        | key == key' = RBNode col left (key', val) right -- same key, change val
        | key >  key' = balance col left (key', val') (insert' right)

-- This is a very beautiful concept in general, capitalizing on the naming of your variables
-- like this to make useful templates. Definitely stealing this in the future.

template a b c d x y z = RBNode Red (RBNode Black a x b) y (RBNode Black c z d)

balance Black (RBNode Red (RBNode Red a x b) y c) z d 
  = template a b c d x y z

balance Black (RBNode Red a x (RBNode Red b y c)) z d
  = template a b c d x y z

balance Black a x (RBNode Red (RBNode Red b y c) z d)
  = template a b c d x y z

balance Black a x (RBNode Red b y (RBNode Red c z d))
  = template a b c d x y z 

balance color a x b = RBNode color a x b

test :: Integer -> Bool

test_tree n = foldl insert empty [(x, x) | x <- [1..n]]

test n = let tree = foldl (insert) empty [(x, x + 5) | x <- [1..n]] in
           let results = map (\x -> case (get tree x) of
                                        Nothing -> False
                                        Just x' -> x == x' - 5) [1..n] in
               foldl (&&) True results
