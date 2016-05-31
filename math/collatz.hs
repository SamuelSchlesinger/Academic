{-# LANGUAGE NoImplicitPrelude #-}

import Delude hiding (iterate, map, takeWhile)
import Data.Stream

f :: Integer -> Integer
f x = case x `mod` 2 of
          0 -> x `quot` 2
          1 -> x * 3 + 1 

collatzOf :: Integer -> Stream Integer
collatzOf n = iterate f n

naturals :: Stream Integer
naturals = iterate (+1) 1

collatz :: Stream (Stream Integer)
collatz = map collatzOf naturals

collatz_until1 :: Stream [Integer]
collatz_until1 = map (takeWhile (/= 1)) collatz

collatz_lengths :: Stream Int
collatz_lengths = map length collatz_until1
