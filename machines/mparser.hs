-- Implementation of algorithm for recognizing regular languges presented in Monoid Machines: a O(logn) parser for regular languages
-- By Armando B. Matos, 2006
-- Code by Kit Freddura, 2016

{- Definitions:
Let (S, *) be a finite monoid, Y a finite alphabet, f: Y -> S and g: S -> {0, 1} be the input and output functions.
f is naturally extended to a monoid homomorphism f: Y* -> S. 
Def: A monoid machine is the 5-tuple (S, *, Y, f, g)
    - A monoid machine recognizes a language L over Y if for all x in Y*, g(f(x)) = 1 iff x in L

Def: f and g
    Given a DFA (Y, S, I, F, D),  the corresponding monoid S
    - for all a in Y let f(a) = Da(q) i.e the function that maps q to D(a,q)
    - for all b in S let g(b) = 1 if b(qo) in F else 0
-}

{- Overview of analysis of work on monoid machine M:
    Input: word x
    Output: 1 if x in L else 0
    compute y = f(a1) * f(a2) * ... * f(an)
    while |y| >= 2 do
        select two adjacent elements a,b of y
        replace ab by a*b
    output g(y)
-}

{- Sequential algorithm 
    Input: DFA recognizing L, a word x = a1a2...an in Y*
    Output: 1 if x in L else 0

    From DFA define:
        - monoid S and it's table
        - the functions g and g
    compute x' = f(a1)*f(a2)*...*f(a3)
    while |x'| >= 2
        select 1 <= 1 < |x'|
        replate aia(i+1) by (ai) * (a(i+1))
    return g(x')
-}


{- example:
 DFA:
    Y = {a,b}
    S = {1, 2}
    I = F = {1}
    D = {(1,b,1), (1,a,2), (2,a,2), (2,b,1)}

S:
    (a, b)
    M(Y) = { fw | w in Y* }
    fe = (1 2)
    fa = (2 2)
    fb = (1 1) 

    table:
        a       b
    a   a       b
    b   a       b

    let x = aaab
    x' = (2 2)(2 2)(2 2)(1 1)
       = (2 2)(2 2)(1 1) : aa -> a
       = (2 2)(1 1): aa -> a
       = (1 1) : ab -> b
    g(b) = fb(1) = 1 -> ACCEPT

    let x = aaba
    x' = (2 2)(2 2)(1 1)(2 2)
       = (2 2)(1 1)(2 2) : aa -> a
       = (1 1)(2 2) : ab -> b
       = (2 2) : ba -> a
    g(a) = fa(1) = 2 -> REJECT
-}
-- CODE --

import qualified Data.Map as Map

-- type alias for dfa transition
type Transition = [(String, Int, Int)]

-- implement DFA for parsing
data DFA = DFA {
    alphabet :: [Char],
    states :: [Int],
    start :: Int,
    delta :: Int -> Char -> Maybe Int, 
    final :: [Int]
}

-- given an element of the alphabet a construct fa in monoid
createElem :: Char -> DFA -> [Maybe Int]
createElem s dfa = map (\x -> (delta dfa) x s) (states dfa)

-- (1 2)(2 3) = (2 3) 
-- takes monoid product of elements
mmul :: [Maybe Int] -> [Maybe Int] -> [Maybe Int]
mmul [] _ = []
mmul (x:xs) g@(y:ys) = case x of
    Just a -> (g !! (a - 1)) : mmul xs g
    Nothing -> Nothing : mmul xs g

-- converts input string to monoid representation
fword :: String -> DFA -> [[Maybe Int]]
fword [] dfa = []
fword (x:xs) dfa 
    | x `elem` (alphabet dfa) = (createElem x dfa) : fword xs dfa
    | otherwise               = (replicate (length $ states dfa) Nothing) : fword xs dfa

-- SEQUENTIAL ALGORITHM
-- parses an input word according to the above algorithm
-- runs currently in O(n + m) time where n is the size of the input word
-- and m is the size of the elements in the monoid
mparse :: [[Maybe Int]] -> DFA -> Bool
mparse (x: []) dfa = case head x of
    Just a  -> elem a (final dfa)
    Nothing -> False
mparse (x:y:xs) dfa = mparse ((mmul x y):xs) dfa

-- mother function, parses from dfa
parse :: String -> DFA -> Bool
parse s dfa = mparse (fword s dfa) dfa

{- Uncomment for test DFA which accepts strings 
ending in b from the alphabet {a,b}+

testdel _ 'a' = Just 2
testdel _ 'b' = Just 1
dfa = DFA "ab" [1,2] 1 testdel [1]

-}
