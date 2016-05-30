{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# LANGUAGE GADTs #-}

-- Typed printf and scanf
-- a variation of
--	The initial view to the typed sprintf and sscanf
--      http://okmij.org/ftp/Haskell/PrintScan.hs

-- This code defines a simple domain-specific language of string
-- patterns and demonstrates two interpreters of the language:
-- for building strings (sprintf) and parsing strings (sscanf).
-- This code thus solves the problem of typed printf/scanf sharing the
-- same format string posed by Chung-chieh Shan.

module PrintScan where

import Prelude hiding ((^))

data I
data V val
data C a b

type family TPrinter functor x
type family TParser  functor x

type instance TPrinter I         x = x
type instance TPrinter (V val)   x = val -> x
type instance TPrinter (C f1 f2) x = TPrinter f1 (TPrinter f2 x)

type instance TParser I         x = x
type instance TParser (V base)  x = (base,x)
type instance TParser (C f1 f2) x = TParser f1 (TParser f2 x)

data F f where
  Lit  :: String -> F I
  Val  :: (Show a, Read a) => F (V a)
  Comp :: F f1 -> F f2 -> F (C f1 f2)

lit :: String -> F I
lit str = Lit str

int :: F (V Int)
int = Val

char :: F (V Char)
char = Val

infixl 5 <>
(<>) :: F f1 -> F f2 -> F (C f1 f2)
(<>) = Comp

printer :: F f -> (String -> a) -> TPrinter f a
printer (Lit str)    k = k str
printer Val          k = \x -> k (show x)
printer (Comp f1 f2) k = printer f1 (\s1 -> printer f2 (\s2 -> k (s1++s2)))

parser :: F f -> a -> String -> Maybe (TParser f a, String)
parser (Lit str) v s = case prefix str s of
			   Nothing   -> Nothing
			   Just s' -> Just (v, s')
parser Val v s       = case reads s of 
                               [(v',s')] -> Just ((v',v),s')
			       _         -> Nothing
parser (Comp f1 f2) v s = case parser f1 v s of
			    Nothing -> Nothing
			    Just (v1,s1) -> parser f2 v1 s1

sprintf :: F functor -> TPrinter functor String
sprintf p = printer p id

sscanf :: String -> F functor  -> Maybe (TParser functor (),String)
sscanf inp fmt = parser fmt ((),inp)



-- Tests

tp1 = sprintf (lit "Hello world")
-- "Hello world"
-- ts1 = sscanf "Hello world" (lit "Hello world")
-- Just ()

tt2 = sprintf (lit "str" <> int) 1
tt3 = sprintf (int <> lit "str") 1


tp2 = sprintf (lit "Hello " <> lit "world" <> char) '!'
-- "Hello world!"
-- ts2 = sscanf "Hello world!" (lit "Hello " <> lit "world" <> char)
-- Just '!'

fmt3 = lit "The value of " <> char <> lit " is " <> int
tp3 = sprintf fmt3 'x' 3
-- "The value of x is 3"
-- ts3 = sscanf "The value of x is 3" fmt3
-- Just ('x',3)

tp4 = sprintf (lit "abc" <> int <> lit "cde") 5
-- "abc5cde"
-- ts4 = sscanf "abc5cde" (lit "abc" <> int <> lit "cde")
-- Just 5

-- The format specification is first-class. One can build format specification
-- incrementally
-- This is not the case with OCaml's printf/scanf (where the 
-- format specification has a weird typing and is not first class).
{-
fmt50 = lit "abc" <> int <> lit "cde" 
fmt5 = fmt50 <> fmt (undefined::Float) <> char
tp5 = sprintf fmt5 5 15 'c'
-- "abc5cde15.0c"
ts5 = sscanf "abc5cde15.0c" fmt5
-- Just (5,15.0,'c')
-}



-- Utility functions

-- A better prefixOf function
-- prefix patt str --> Just str'
--    if the String patt is the prefix of String str. The result str'
--    is str with patt removed
-- Otherwise, the result is Nothing

prefix :: String -> String -> Maybe String
prefix "" str = Just str
prefix (pc:pr) (sc:sr) | pc == sc = prefix pr sr
prefix _  _  = Nothing


