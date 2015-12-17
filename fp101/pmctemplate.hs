module Lab5 where

import Control.Monad

-- A computation in the concurrency monad is a function whose
-- first argument is its continuation

data Concurrent a = Concurrent ((a -> Action) -> Action)

data Action
    = Atom (IO Action)   -- an atomic computation, returning a new action
    | Fork Action Action -- create a new thread
    | Stop               -- terminate this thread

instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"

-- ===================================
-- Ex. 0
-- ===================================

-- transforms a ((a -> Action) -> Action) into an Action
-- that uses Stop :: Action to create the continuation to
-- the Concurrent a passed as the first argument to action.
action :: Concurrent a -> Action
action (Concurrent comp)   = comp (\_ -> Stop)

--action :: ((a -> Action) -> Action) -> Action
--action  f = f  (const Stop)


-- ===================================
-- Ex. 1
-- ===================================

-- makes a continuation, which gets discarded, and then
-- it returns a Stop action.
stop :: Concurrent a
stop  = Concurrent (\_ -> Stop)

--stop' :: (a -> Action) -> Action
--stop' _ =  Stop

-- ===================================
-- Ex. 2
-- ===================================

-- turns an arbitrary computation in the IO Monad into an atomic
-- action represented using the Atom constructor.
atom :: IO a -> Concurrent a
atom ia =  Concurrent (atom' ia)
-- Concurrent (\c -> Atom (ia >>= \a -> return (c, a)))

atom' :: IO a -> (a -> Action) -> Action
atom' ia c = Atom (do  a <- ia
                       return (c a))

-- maybe implement with LiftM
--liftM :: Monad m => (a -> b) -> m a -> m b
--liftM c ia = do a <- ia
--                 return (c a)


-- ===================================
-- Ex. 3
-- ===================================

-- forks a computation by turning it into an action and continues
-- by passing () as the input to the continuation:
fork :: Concurrent a -> Concurrent ()
fork comp =  Concurrent (\cnt -> Fork (action comp) (cnt ()))
-- like void

--fork' :: a -> (() -> Action) -> Action
--fork' comp = \cnt -> Fork (action comp) (cnt ())

-- combines two computations into one by forking them both
-- and passing the given continuation to both parts
par :: Concurrent a -> Concurrent a -> Concurrent a
par (Concurrent compa) (Concurrent compb) =
  Concurrent (\cnt -> Fork (compa cnt) (compb cnt))


-- ===================================
-- Ex. 4
-- ===================================

instance Monad Concurrent where
  (Concurrent f) >>= g = Concurrent (\c -> (f (\x -> case g x of
                                                (Concurrent b) -> b c)))
  
  return x = Concurrent (\c -> c x)



bind :: ((a -> Action) -> Action) -> (a -> ((b -> Action) -> Action)) -> ((b -> Action) -> Action)
bind ma g =   \c -> ma (\a -> (g a) c)

extract :: Concurrent a -> (a -> Action) -> Action
extract (Concurrent a) = a
--ma :: (a -> Action)-> Action
--ma a = \c -> _fn

--f :: a -> (b -> Action) -> Action
--f ac = undefined ac


-- ===================================
-- Ex. 5
-- ===================================

roundRobin :: [Action] -> IO ()
roundRobin [] = return ()
roundRobin (a:as) = case a of
  Stop      -> roundRobin as
  Fork x y  -> roundRobin (x : y : as)
  Atom x    -> do y <- x
                  roundRobin (as ++ [y])

-- ===================================
-- Tests
-- ===================================

ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331)
         loop $ genRandom 42
         atom (putStrLn "")


-- ===================================
-- Helper Functions
-- ===================================

run :: Concurrent a -> IO ()
run x = roundRobin [action x]

genRandom :: Int -> [Int]
genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

loop :: [Int] -> Concurrent ()
loop xs = mapM_ (atom . putStr . show) xs


-- Exercises

-- Ex (0)
act0 :: Action
act0 = action (Concurrent (\a -> Stop))

-- Ex (1) -- type :: Action


-- Ex (2)
act2 :: Action
act2 = action (Concurrent (\a -> Fork Stop $ Fork Stop Stop))
-- fork stop fork stop stop

-- Ex (3)
-- won't compile
-- act3 = action  (Concurrent (\a -> Atom $ putStr "Haskell"))

-- Ex (4)
act4 :: Action
act4 = action (Concurrent (\a -> Atom $ putStr "Haskell" >> return Stop))
-- atom

-- Ex (5)
con5 :: Concurrent a
con5 = Concurrent (\a -> Atom $ putStr "Haskell" >> return Stop)


-- Ex (6)
act6 :: Action
act6 = action stop
-- stop

-- Ex (7)
con7 :: Concurrent a
con7 = stop
-- stop :: Concurrent a

-- Ex (8)
act8 :: Action
act8 = action . atom . putStrLn $ "Haskell"
-- Error

-- Ex (9)
act9 :: Action
act9 = action $ atom undefined
-- atom

-- Ex (10)
con10 :: Concurrent ()
con10 = atom . putStrLn $ "Haskell"
-- Error

-- Ex (11)
act11 :: Action
act11 = action $ fork stop
-- fork stop stop

-- Ex (12)
act12 :: Action
act12 = action (fork (atom (putStr "Hacker")))
-- fork atom stop

-- Ex (13)  act12 :: Action

-- Ex (14)

act14 :: Action
act14 = action (fork undefined)
-- error

-- Ex (15)
act15 :: Action
act15 = action $ par stop stop
-- fork stop stop

-- Ex (16)
act16 :: Action
act16 = action (par (atom (putStr "think")) (atom (putStr "hack")))
-- fork atom atom

-- Ex (17)
act17 :: Action
act17 = action (par stop $ fork stop)
-- fork stop fork stop stop

-- Ex (18)
act18 :: Action
act18 = action $ par (atom $ putChar 'x') (fork stop)

-- Ex (19)
act19 :: Action
act19 = action (stop >>= (\c -> stop))

-- Ex (20)
--act20 = action (atom (putStrLn "whatever...") >>= stop)
-- won't compile

-- Ex (21)
-- act20 = stop >>=  stop
-- won't compile

-- Ex (22) -- won't compile

-- Ex (23)
act22 :: Action
act22 = action (fork stop >>= \_ -> fork stop)
-- fork stop fork stop stop

-- Ex (24)
io24 :: IO ()
io24 = run ex0


-- Ex (25)
