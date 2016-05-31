{-# LANGUAGE NoImplicitPrelude #-}

import Delude hiding (iterate, map, takeWhile)
import Data.Stream

f :: Integer -> Integer
f x = if x `mod` 2 == 0 then x `quot` 2 else x * 3 + 1

collatz :: Integer -> Stream Integer
collatz n = iterate f n

allCollatz :: Stream (Stream Integer)
allCollatz = map collatz $ iterate (+1) 1

allCollatzTil1 :: Stream [Integer]
allCollatzTil1 = map (takeWhile (/= 1)) allCollatz

allCollatzTil1Lengths :: Stream Int
allCollatzTil1Lengths = map length allCollatzTil1
