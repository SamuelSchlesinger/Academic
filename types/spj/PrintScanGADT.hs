{-# LANGUAGE GADTs, TypeFamilies, EmptyDataDecls, UndecidableInstances #-}

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
import Data.Char


-- Start TPrinter
type SPrintf f = TPrinter f String

type family TPrinter f x
type instance TPrinter L         x = x
type instance TPrinter (V val)   x = val -> x
type instance TPrinter (C f1 f2) x = TPrinter f1 (TPrinter f2 x)
-- Stop TPrinter


-- Start fmt
data F f where
  Lit :: String -> F L
  Val :: Parser val -> Printer val -> F (V val)
  Cmp :: F f1 -> F f2 -> F (C f1 f2)

data L
data V val
data C f1 f2

type Parser  a = String -> [(a,String)]
type Printer a = a -> String
-- Stop fmt


lit :: String -> F L
lit str = Lit str

int :: F (V Int)
int = Val reads show

char :: F (V Char)
char = Val read_char show_char
     where
       read_char []     = []
       read_char (c:cs) = [(c,cs)]
       show_char c      = [c]

string :: F (V String)
-- Printing: show the string without quotes
-- Parsing: match until white space
string = Val read_string show_string
     where
       read_string s = [break (not . isSpace) s]
       show_string s = s

infixl 5 <>
(<>) :: F f1 -> F f2 -> F (C f1 f2)
(<>) = Cmp

-- Start printer
sprintf :: F f -> SPrintf f 
sprintf p = printer p id

printer :: F f -> (String -> a) -> TPrinter f a
printer (Lit str)    k = k str
printer (Val _ show) k = \x -> k (show x)
printer (Cmp f1 f2)  k = printer f1 (\s1 -> 
                         printer f2 (\s2 -> 
                         k (s1++s2)))
-- Stop printer

-- Start parser
type SScanf f = String -> Maybe (TParser f (), String)

type family TParser f x
type instance TParser L         x = x
type instance TParser (V val)   x = (x,val)
type instance TParser (C f1 f2) x = TParser f2 (TParser f1 x)

sscanf :: F f -> SScanf f
sscanf fmt inp = parser fmt () inp

parser :: F f -> a -> String -> Maybe (TParser f a, String)
parser (Lit str)     v s = parseLit str v s
parser (Val reads _) v s = parseVal reads v s
parser (Cmp f1 f2)   v s = case parser f1 v s of
			     Nothing -> Nothing
			     Just (v1,s1) -> parser f2 v1 s1

parseLit :: String -> a -> String -> Maybe (a, String)
parseLit str v s = case prefix str s of
			Nothing -> Nothing
			Just s' -> Just (v, s')

parseVal :: Parser b -> a -> String -> Maybe ((a,b), String)
parseVal reads v s = case reads s of 
                       [(v',s')] -> Just ((v,v'),s')
                       _         -> Nothing
-- Stop parser


-- Tests

tp1 = sprintf (lit "Hello world")
-- "Hello world"
ts1 = sscanf (lit "Hello world") "Hello world"
-- Just ((),"")

tt2 = sprintf (lit "str" <> int) 1
tt3 = sprintf (int <> lit "str") 1


tp2 = sprintf (lit "Hello " <> lit "world" <> char) '!'
-- "Hello world!"
ts2 = sscanf (lit "Hello " <> lit "world" <> char) "Hello world!"
-- Just (((),'!'),"")

fmt3 = lit "The value of " <> char <> lit " is " <> int
tp3 = sprintf fmt3 'x' 3
-- "The value of x is 3"
ts3 = sscanf fmt3 "The value of x is 3"
-- Just ((((),'x'),3),"")

tp4 = sprintf (lit "abc" <> int <> lit "cde") 5
-- "abc5cde"
ts4 = sscanf (lit "abc" <> int <> lit "cde") "abc5cde"
-- Just (((),5),"")

-- The format specification is first-class. One can build format specification
-- incrementally
-- This is not the case with OCaml's printf/scanf (where the 
-- format specification has a weird typing and is not first class).
{-
fmt50 = lit "abc" <> int <> lit "cde" 
fmt5 = fmt50 <> fmt (undefined::Float) <> char
tp5 = sprintf fmt5 5 15 'c'
-- "abc5cde15.0c"
ts5 = sscanf fmt5 "abc5cde15.0c"
-- Just (5,15.0,'c')
-}



-- Utility functions

-- A better prefixOf function
-- prefix patt str --> Just str'
--    if the String patt is the prefix of String str. 
--    The result str' is str with patt removed
-- Otherwise, the result is Nothing

prefix :: String -> String -> Maybe String
prefix "" str = Just str
prefix (pc:pr) (sc:sr) | pc == sc = prefix pr sr
prefix _  _  = Nothing


-- Start Dollars
newtype Dollars = MkD Int

dollars :: F (V Dollars)
dollars = Val read_dol show_dol
  where
    read_dol ('$':s) = [ (MkD d, s) | (d,s) <- reads s ]
    read_dol _       = []
    show_dol (MkD d) = '$' : show d
-- Stop Dollars
