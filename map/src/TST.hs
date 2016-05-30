{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}

module TST (T(..), insert, snag, empty) where 

import Data.Binary
import GHC.Generics (Generic)

data T c v where
    N :: T c v -> T c v -> (c, Maybe v) -> T c v -> T c v
    E :: T c v deriving (Generic)

instance (Binary c, Binary v) => Binary (T c v)

insert :: (Ord c) => T c v -> ([c], v) -> T c v

empty = E

snag :: (Ord c) => T c v -> [c] -> Maybe v

-- | This took me a weirdly long amount of time to reason about.
-- | Ended up being far shorter and more coherent than the Java version.

insert _ ([], _) = E -- if you're trying to insert the empty list you suck

insert E ((c:[]), v) = N E E (c, (Just v)) E 

insert E ((c:cs), v) = N E (insert E (cs, v)) (c, Nothing) E

insert (N l d (c', v') r) (s@(c:[]), v)
  | c <  c' = (N (insert l (s, v)) d (c', v') r) -- go left
  | c == c' = (N l d (c, (Just v)) r) -- slap 'er down
  | c >  c' = (N l d (c', v') (insert r (s, v))) -- go right

insert (N l d (c', v') r) (s@(c:cs), v)
  | c <  c' = (N (insert l (s, v)) d (c', v') r) -- go left
  | c == c' = (N l (insert d (cs, v)) (c', v') r) -- go down
  | c >  c' = (N l d (c', v') (insert r (s, v))) -- go right

snag _ [] = Nothing -- wat is nun?

snag E _  = Nothing -- heh?

snag (N l d (c', v') r) s@(c:[])
  | c <  c' = snag l s -- go left
  | c == c' = v' -- return dat thang
  | c >  c' = snag r s -- go right

snag (N l d (c', v') r) s@(c:cs)
  | c <  c' = snag l s -- g'left
  | c == c' = snag d cs -- g'down
  | c >  c' = snag r s --g'right

test_input = [(x ++ y ++ z, x ++ y ++ z) | x <- ["hi", "ho", "he", "ca", "cu", "ke", "pu", "pi"], y <- ["bi", "bo", "bu", "ba", "be", "la", "ar"], z <- ["re", "ro", "ra", "rq", "rk"]]

test_tst = foldl insert E test_input

test = let input = map fst test_input 
           in let results = map (snag test_tst) input
                  in (map (\(Just x) -> x)  results) == input
