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
data A base
data C a b

type family TPrinter functor x
type family TParser  functor x

type instance TPrinter I x = x
type instance TPrinter (A base) x = base -> x
type instance TPrinter (C f1 f2) x = TPrinter f2 (TPrinter f1 x)

type instance TParser I x = x
type instance TParser (A base) x = (base,x)
type instance TParser (C f1 f2) x = TParser f1 (TParser f2 x)

data F functor = 
    F{printer :: forall a. (String -> a) -> TPrinter functor a,
      parser  :: forall a. (a,String) -> Maybe (TParser functor a,String)}


lit :: String -> F I
lit str = F{printer = \k -> k str, 
	    parser = \(x,inp) -> 
	               maybe Nothing (\inp' -> Just (x,inp')) $ prefix str inp}

int :: F (A Int)
int   = F{printer = \k -> \x -> k (show (x::Int)),
	  parser = \(x,inp) -> maybe Nothing (\(x',s')->Just ((x',x),s')) $ 
	                       genparse inp}

char :: F (A Char)
char   = F{printer = \k -> \x -> k [x],
	   parser = \(x,inp) ->
	               case inp of
			 c:inp -> Just ((c,x),inp)
			 _     -> Nothing}

infixl 5 ^
(^) :: F f1 -> F f2 -> F (C f2 f1)
p1 ^ p2 = F{printer = \k -> printer p1 (\s1 -> printer p2(\s2 -> k (s1 ++ s2))),
	    parser = parse}
 where parse x = maybe Nothing (parser p2) (parser p1 x)

sprintf :: F functor -> TPrinter functor String
sprintf p = printer p id

sscanf :: String -> F functor  -> Maybe (TParser functor (),String)
sscanf inp fmt = parser fmt ((),inp)



-- Tests

tp1 = sprintf (lit "Hello world")
-- "Hello world"
ts1 = sscanf "Hello world" (lit "Hello world")
-- Just ()

tt2 = sprintf (lit "str" ^ int) 1
tt3 = sprintf (int ^ lit "str") 1


tp2 = sprintf (lit "Hello " ^ lit "world" ^ char) '!'
-- "Hello world!"
ts2 = sscanf "Hello world!" (lit "Hello " ^ lit "world" ^ char)
-- Just '!'

fmt3 = lit "The value of " ^ char ^ lit " is " ^ int
tp3 = sprintf fmt3 'x' 3
-- "The value of x is 3"
ts3 = sscanf "The value of x is 3" fmt3
-- Just ('x',3)

tp4 = sprintf (lit "abc" ^ int ^ lit "cde") 5
-- "abc5cde"
ts4 = sscanf "abc5cde" (lit "abc" ^ int ^ lit "cde")
-- Just 5

-- The format specification is first-class. One can build format specification
-- incrementally
-- This is not the case with OCaml's printf/scanf (where the 
-- format specification has a weird typing and is not first class).
{-
fmt50 = lit "abc" ^ int ^ lit "cde" 
fmt5 = fmt50 ^ fmt (undefined::Float) ^ char
tp5 = sprintf fmt5 5 15 'c'
-- "abc5cde15.0c"
ts5 = sscanf "abc5cde15.0c" fmt5
-- Just (5,15.0,'c')
-}



-- Utility functions

{-
-- Primitive Printer/parsers
showread :: (Show a, Read a) => PrinterParser a
showread = PrinterParser show parse
 where
 parse s = case reads s of
	     [(v,s')] -> Just (v,s')
	     _        -> Nothing
-}

genparse s = case reads s of
	     [(v,s')] -> Just (v,s')
	     _        -> Nothing

-- A better prefixOf function
-- prefix patt str --> Just str'
--    if the String patt is the prefix of String str. The result str'
--    is str with patt removed
-- Otherwise, the result is Nothing

prefix :: String -> String -> Maybe String
prefix "" str = Just str
prefix (pc:pr) (sc:sr) | pc == sc = prefix pr sr
prefix _  _  = Nothing



{-
 -- Old code, just for reference

-- Printer/parsers (injection/projection pairs)
data PrinterParser a = PrinterParser (a -> String) (String -> Maybe (a,String))

fmt :: (Show b, Read b) => b -> F a (b -> a)
fmt x = FPP showread

intp (FLit str) k = k str
intp FInt       k = \x -> k (show x)
intp FChr       k = \x -> k [x]
intp (FPP (PrinterParser pr _))  k = \x -> k (pr x)
intp (a :^ b)   k = intp a (\sa -> intp b (\sb -> k (sa ++ sb)))


-- The interpreter for scanf

ints :: F a b -> String -> b -> Maybe (a,String)
ints (FLit str) inp x = maybe Nothing (\inp' -> Just (x,inp')) $ prefix str inp
ints FChr (c:inp) f = Just (f c,inp)
ints FChr "" f      = Nothing
ints (FPP (PrinterParser _ pa)) inp f = 
    maybe Nothing (\(v,s) -> Just (f v,s)) $ pa inp 
ints FInt inp f = ints (FPP showread) inp f
ints (a :^ b) inp f = maybe Nothing (\(vb,inp') -> ints b inp' vb) $ 
		       ints a inp f


sprintf :: F String b -> b
sprintf fmt = intp fmt id

sscanf :: String -> F a b -> b -> Maybe a
sscanf inp fmt f = maybe Nothing (Just . fst) $ ints fmt inp f
-}
