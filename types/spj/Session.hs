{-# LANGUAGE TypeFamilies #-}
module Session where

-- Start init
data Stop = Done
newtype In  a b = In (a -> IO b)
data    Out a b = Out a   (IO b)

add_server :: In Int (In Int (Out Int Stop))
add_server = In $ \x -> return $ In $ \y ->
             do { putStrLn "Thinking"
                ; return $ Out (x + y) (return Done) }
-- Stop init

-- Start class
class Session a where
  type Dual a
  run :: a -> Dual a -> IO ()
-- Stop class

-- Start instances
instance (Session b) => Session (In a b) where
  type Dual (In a b) = Out a (Dual b)
  run (In f) (Out a d) = f a >>= \b -> d >>= \c -> run b c

instance (Session b) => Session (Out a b) where
  type Dual (Out a b) = In a (Dual b)
  run (Out a d) (In f) = f a >>= \b -> d >>= \c -> run c b

instance Session Stop where
  type Dual Stop = Stop
  run Done Done = return ()
-- Stop instances

-- Start choice
instance (Session a, Session b) => Session (Either a b) where
  type Dual (Either a b) = (Dual a, Dual b)
  run (Left  y) (x,_) = run y x
  run (Right y) (_,x) = run y x

instance (Session a, Session b) => Session (a, b) where
  type Dual (a,b) = Either (Dual a) (Dual b)
  run (x,_) (Left  y) = run x y
  run (_,x) (Right y) = run x y
-- Stop choice

-- Start add_client
add_client :: Out Int (Out Int (In Int Stop))
add_client = Out 3 $ return $ Out 4 $ 
             do { putStrLn "Waiting"
                ; return $ In $ \z -> print z >> return Done }
-- Stop add_client

-- Start neg_server
neg_server :: In Int (Out Int Stop)
neg_server = In $ \x ->
             do { putStrLn "Thinking"
                ; return $ Out (-x) (return Done) }
-- Stop neg_server
neg_client :: Out Int (In Int Stop)
neg_client = Out 5 . (putStrLn "Waiting" >>) . return
           $ In $ \z -> print z >> return Done

-- Start combined
server :: (In Int (Out Int Stop),
           In Int (In Int (Out Int Stop)))
server = (neg_server, add_server)

client :: Either (Out Int (In Int Stop))
                 (Out Int (Out Int (In Int Stop)))
client = Right add_client
-- Stop combined


test1 = run add_server add_client
test2 = run add_client add_server
test = run server client >> run client server
