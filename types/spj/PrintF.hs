{-# OPTIONS -fglasgow-exts #-}

-- Typed printf and scanf
-- scanf is future work..

module PrintF where

import Prelude hiding ((^))

-- I want to write the following, but I can't. A bummer
{-
class DeepApply a where
    type Result a :: * -> *
    deep_apply :: a -> (String -> b) -> Result String b

instance DeepApply String where
    type Result String b = b
    deep_apply s f = f s

instance DeepApply c => DeepApply (a -> c) where
    type Result (a -> c) b = a -> Result c b
    deep_apply x f = \a -> deep_apply (x a) f
-}

-- I have to work around as follows

type family TApply functor x
type instance TApply I x = x
type instance TApply (a -> c) x = a -> TApply c x

data I

infixl 5 ^

class FCompose a where
    type Result a
    (^) :: (String -> a) -> (String -> b) -> (String -> TApply (Result a) b)

instance FCompose String where
    type Result String = I
    (^) f1 f2 = \s -> f2 (f1 s)

instance FCompose c => FCompose (a -> c) where
    type Result (a -> c) = a->Result c
    (^) f1 f2 = \s -> \x -> ((\s -> f1 s x) ^ f2) s

lit :: String -> (String -> String)
lit str = \s -> s ++ str

int :: String -> Int -> String
int = \s -> \x -> s ++ show x

char :: String -> Char -> String
char = \s -> \x -> s ++ [x]

sprintf:: (String -> t) -> t
sprintf fmt = fmt ""



-- Tests

tp1 = sprintf (lit "Hello world")
-- "Hello world"

-- ts1 = sscanf "Hello world" (lit "Hello world")
-- Just ()

tt2 = sprintf (lit "str" ^ int) 1
tt3 = sprintf (int ^ lit "str") 1


tp2 = sprintf (lit "Hello " ^ lit "world" ^ char) '!'
-- "Hello world!"
-- ts2 = sscanf "Hello world!" (lit "Hello " ^ lit "world" ^ char)
-- Just '!'

fmt3 = lit "The value of " ^ char ^ lit " is " ^ int
tp3 = sprintf fmt3 'x' 3
-- "The value of x is 3"
-- ts3 = sscanf "The value of x is 3" fmt3
-- Just ('x',3)

tp4 = sprintf (lit "abc" ^ int ^ lit "cde") 5
-- "abc5cde"
-- ts4 = sscanf "abc5cde" (lit "abc" ^ int ^ lit "cde")
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

-- examples used in the paper
paper_examples = and [
     sprintf (lit "day")		         == "day",
     sprintf (lit "day" ^ lit "s")               == "days",
     sprintf (lit "day " ^ int) 3                == "day 3",
     sprintf (int ^ lit " day" ^ lit "s") 3      == "3 days"
	      ]




-- Old code, for reference
{-
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
-}

