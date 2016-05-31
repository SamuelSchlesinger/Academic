# Delude

To install: 

> cabal install delude

To use:

> {-# LANGUAGE NoImplicitPrelude #-}

> import Delude

Why?

In my maths courses, often we'll write on the board something like f + g to denote
the function f(x) + g(x). As well, we might write (>= 5) && (<= 10) to denote the
statement that some x is greater than or equal to 5 and less than or equal to 10. I've
inductively generalized parts of the Numeric hierarchy and the Bool interface to allow
for this sort of thing. At the same time, I didn't want to cause any old Haskell programs
which are correct to stop compiling, so the Delude should compile a superset of programs
which would compile with the regular Prelude.
