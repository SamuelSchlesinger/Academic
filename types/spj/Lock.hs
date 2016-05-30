{-# LANGUAGE TypeFamilies, EmptyDataDecls, ScopedTypeVariables #-}
module Lock where
import Arithm (Zero, Succ, Nat(toInt), One, Two)

-- Start PMonad
class PMonad m where
  unit :: a -> m p p a
  bind :: m p q a -> (a -> m q r b) -> m p r b
-- Stop PMonad

-- Start phantoms
data Nil
data Cons l s

data Locked
data Unlocked
-- Stop phantoms

-- We also lift a few i/o operations to LockM

-- Start actions
newtype LockM p q a = LockM { unLockM :: IO a }

instance PMonad LockM where
  unit x   = LockM (return x)
  bind m k = LockM (unLockM m >>= unLockM . k)
-- Stop actions

-- Start lput
lput :: String -> LockM p p ()
lput = LockM . putStrLn
-- Stop lput

-- Start GetSet
type family Get n p
type instance Get Zero (Cons e p) = e
type instance Get (Succ n) (Cons e p) = Get n p

type family Set n e' p
type instance Set Zero e' (Cons e p) = Cons e' p
type instance Set (Succ n) e' (Cons e p) = Cons e (Set n e' p)
-- Stop GetSet

-- In the real program, the following will execute actions to acquire
-- or release a lock. Here, we just print out our intentions.

-- Start Lock
newtype Lock n = Lock Int deriving Show

mkLock :: forall n. Nat n => Lock n
mkLock = Lock (toInt (undefined::n))
-- Stop Lock
-- Start lock1
lock1 = mkLock :: Lock One
-- Stop lock1

-- Start locking
acquire :: (Get n p ~ Unlocked) => 
	   Lock n -> LockM p (Set n Locked p) ()
acquire l = LockM (putStrLn ("acquire " ++ show l))

release :: (Get n p ~ Locked) => 
	   Lock n -> LockM p (Set n Unlocked p) ()
release l = LockM (putStrLn ("release " ++ show l))
-- Stop locking

-- Start run
type ThreeLocks = Cons Unlocked (Cons Unlocked (Cons Unlocked Nil))
run :: LockM ThreeLocks ThreeLocks a -> IO a
run = unLockM
-- Stop run

-- Start with1
with1 a = acquire lock1 `bind` \_ ->
          a `bind` \x ->
          release lock1 `bind` \_ ->
          unit x
-- Stop with1

test1 = run (with1 (lput "hello"))

{-

*Lock> test1
acquire Lock 1
hello
release Lock 1

-}


-- test2 = run (with1 (with1 (unit True)))
{-
The above causes the expected type error:
    Couldn't match expected type `Unlocked'
           against inferred type `Get One (Set One Locked ThreeLocks)'
-}

-- test3 = run (acquire lock1)
{-
The above too raises the expected error:

    Couldn't match expected type `Unlocked'
           against inferred type `Locked'
      Expected type: LockM ThreeLocks ThreeLocks a
      Inferred type: LockM ThreeLocks (Set One Locked ThreeLocks) ()
    In the first argument of `run', namely `(acquire lock1)'
    In the expression: run (acquire lock1)
-}

-- But the following is OK

test21 = run (with1 (release lock1 `bind` \_ -> 
	              with1 (lput "here") `bind` \_ -> acquire lock1))
{-
*Lock> test21
acquire Lock 1
release Lock 1
acquire Lock 1
here
release Lock 1
acquire Lock 1
release Lock 1
-}


-- Locks may be acquired and released in arbitrary, not necessarily LIFO,
-- order
with02 a = acquire_locks `bind` \_ ->
          a `bind` \x ->
          release_locks `bind` \_ ->
          unit x
 where
 -- We acquire and release in the same order. Not a LIFO
 acquire_locks = acquire (mkLock::Lock Zero) `bind` \_ -> 
		 acquire (mkLock::Lock Two)
 release_locks = release (mkLock::Lock Zero) `bind` \_ ->
		 release (mkLock::Lock Two)

test4 = run (with02 (lput "hello again"))

{-
*Lock> test4
acquire Lock 0
acquire Lock 2
hello again
release Lock 0
release Lock 2
-}

test5 = run (with02 (with1 (lput "hello again")))

{-
*Lock> test5
acquire Lock 0
acquire Lock 2
acquire Lock 1
hello again
release Lock 1
release Lock 0
release Lock 2
-}

{- The following is an error, too
test6 = run (acquire (mkLock::Lock One) `bind` \_ -> release (mkLock::Lock Two))
-}

-- We can define actions that do not change the lock state but require
-- a certain lock being held

-- Start critical1
critical1 :: (Get One p ~ Locked) => LockM p p ()
critical1 = LockM (putStrLn "Critical section 1")
-- Stop critical1

-- test8 = run critical1
{-
Expected type error:
    Couldn't match expected type `Locked'
           against inferred type `Get One ThreeLocks'
-}
test7 = run (with1 critical1)
{-
*Lock> test7
acquire Lock 1
Critical section 1
release Lock 1
-}
