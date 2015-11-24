-- | Homework 8 -- Interactive Programs

module Homework08 where
import Data.Char (toUpper)

-- Ex (1)  - putStr

putStr' :: String -> IO ()

-- bad  -- wrong type, won't compile
-- putStr1 []     = return ""
-- putStr1 (x:xs) = putChar x >> putStr1 xs

-- good, works
putStr' []      = return ()
putStr' (x:xs)  = putChar x >> putStr' xs

-- bad, won't compile
--putStr3 []      = return ()
--putStr3 (x:xs)  = putChar x >>= putStr3 xs

-- bad - won't compile
--putStr4 []        = return ()
--putChar4 (x:xs)   = putStr4 xs >>= putChar x


-- Ex (2)  - putStrLn

putStrLn1,putStrLn2,putStrLn3,putStrLn5
  :: String -> IO ()

-- Good
putStrLn1 []  = putChar '\n'
putStrLn1 xs  = putStr' xs >> putStrLn1 ""

-- Good
putStrLn2 []  = putChar '\n'
putStrLn2 xs  = putStr' xs >> putChar '\n'

-- Good
putStrLn3 []  = putChar '\n'
putStrLn3 xs  = putStr' xs >>= \ x -> putChar '\n'

-- Bad, won't compile
--putStrLn4 []  = putChar '\n'
--putStrLn4 xs  = putStr' xs >> \ x -> putChar '\n'

-- Good
putStrLn5 []  = putChar '\n'
putStrLn5 xs  = putStr' xs >> putStr' "\n"

-- bad -- in fun it loop
--putStrLn6 []  = putChar '\n'
--putStrLn6 xs  = putStr' xs >> putStrLn6 "\n"

-- bad - won't compile
--putStrLn7 []  = return ""
--putStrLn7 xs  = putStrLn7 xs >> putStr' "\n"

-- bad -- won't compile
--putStrLn8 []  = putChar "\n"
--putStrLn8 xs  = putStr' xs >> putChar '\n'


-- Ex (3)  -- getLine

getLine1,getLine2,getLine3,getLine4
  :: IO String

-- bad, doesn't ignore trailing spaces
getLine1 = get1 ""

get1 :: String -> IO String
get1 xs
  = do x <- getChar
       case x of
         ' ' -> return xs
         '\n' -> return xs
         _ -> get1 (xs ++ [x])


-- bad - reverses input
getLine2 = get2 ""

get2 :: String -> IO String
get2 xs
  = do x <- getChar
       case x of
         '\n' -> return xs
         _ -> get2 (x:xs)


-- good, good, good
getLine3  = get3 []

get3 :: String -> IO String
get3 xs
  = do x <- getChar
       case x of
         '\n'  -> return xs
         _ -> get3 (xs ++ [x])

-- bad, new line comes first.
getLine4  = get4 []

get4 :: String -> IO String
get4 xs
  = do x <- getChar
       case x of
         '\n' -> return (x:xs)
         _ -> get4 (xs ++ [x])

testGetLine ::  IO ()
testGetLine = do x <- getLine4
                 putStrLn x


-- Ex (4) - interact

interact1,interact2,interact4
  :: (String -> String) -> IO ()

-- good
interact1 f
  = do input <- getLine
       putStrLn1 (f input)

-- bad - doesn't run 'f'
interact2 f
  = do input <- getLine
       putStrLn1 input

-- bad - doesn't compile
--interact3 f
--  = do input <- getChar
--       putStrLn (f input)

-- bad - no new line
interact4 f
  = do input <- getLine
       putStr (f input)

-- nice testing funtion of interact
upper :: String -> String
upper  = map toUpper



-- Ex (5)  - Sequence function

sequence_2,sequence_4,sequence_5,sequence_7
  :: Monad m => [m a] -> m ()

-- bad -  won't compile, bad base case type
--sequence_1 [] = return []
--sequence_1 (m:ms) = m >> \ _ -> sequence_1 ms

-- good
sequence_2 [] = return ()
sequence_2 (m:ms) = (foldl (>>) m ms) >> return ()

-- bad -  wrong type - but still a sequence ?
sequence_3 :: Monad m => [m ()] -> m ()
sequence_3 ms = foldl (>>) (return ()) ms

-- good
sequence_4 [] = return ()
sequence_4 (m : ms) = m >> sequence_4 ms

-- good
sequence_5 [] = return ()
sequence_5 (m:ms) = m >>= \ _ -> sequence_5 ms

-- bad - won't compile
-- sequence_6 ms = foldr (>>=) (return ()) ms

-- good
sequence_7 ms = foldr (>>) (return ()) ms

-- bad  - wrong type
sequence_8 :: Monad m => [m a] -> m [a]
sequence_8 ms = foldr (>>) (return []) ms


-- Ex (6)  - sequence

sequence1,sequence5,sequence8
  :: Monad m => [m a] -> m [a]

-- good
sequence1 [] = return []
sequence1 (m:ms)
    = m >>=
      \ a ->
        do as <- sequence1 ms
           return (a:as)

-- bad - won't compile
-- sequence2 ms = foldr func (return ()) ms
--   where
--     func :: (Monad m) => m a -> m [a] -> m [a]
--     func m acc
--       = do x <- m
--            xs <- acc
--            return (x:xs)

-- bad
-- sequence3 ms = foldr func (return []) ms
--    where
--      func :: (Monad m) => m a -> m [a] -> m [a]
--      func m acc = m : acc

-- bad - won't compile
-- sequence4 [] = return []
-- sequence4 (m:ms) = return (a:as)
--   where
--     a <- m
--     as <- sequence' ms

-- good
sequence5 ms = foldr func (return []) ms
  where
         func :: (Monad m) => m a -> m [a] -> m [a]
         func m acc
           = do x <- m
                xs <- acc
                return (x:xs)

-- bad  - won't compile
-- sequence6 [] = return []
-- sequence6 (m:ms)
--   = m >>
--       \ a ->
--         do as <- sequence6 ms
--            return (a:as)

-- bad  - won't compile
-- sequence' [] = return []
-- sequence' (m:ms) = m >>= \a ->
--     as <- sequence' ms
--     return (a:ms)

-- good
sequence8 [] = return []
sequence8 (m:ms)
  = do a <- m
       as <- sequence8 ms
       return (a:as)


-- Ex (8)  - mapM'

mapM1,mapM2,mapM6,mapM7,mapM8
  :: Monad m => (a -> m b) -> [a] -> m [b]

-- good
mapM1 f as = sequence1 (map f as)

-- good
mapM2 f [] = return []
mapM2 f (a:as)
  = f a >>= \ b -> mapM2 f as >>= \ bs -> return (b:bs)

-- bad  - wrong type - mapM_
mapM3 :: Monad m => (a -> m b) -> [a] -> m ()
mapM3 f as = sequence_2 (map f as)

-- bad  - won't compile
-- mapM4 f [] = return []
-- mapM4 f (a:as)
--   = f a >> \ b -> mapM4 f as >> \ bs -> return (b:bs)

-- bad  - won't compile
-- mapM5 f [] = return []
-- mapM5 f (a:as) =
-- do
--   f a -> b
--   mapM5 f as -> bs
--   return (b:bs)

--  good
mapM6 f [] = return []
mapM6 f (a:as)
  = do b <- f a
       bs <- mapM6 f as
       return (b:bs)

-- good
mapM7 f [] = return []
mapM7 f (a:as)
  = f a >>=
    \ b ->
      do bs <- mapM7 f as
         return (b:bs)

-- good, but marked bad
mapM8 _ [] = return []
mapM8 f (a:as)
  = f a >>=
  	  \ b ->
  	    do bs <- mapM8 f as
  	       return (bs ++ [b])

-- test mapM
-- mapM putChar ['a','b','c']
-- mapm putStr ["a","bc","def"]


-- Ex (8)  - filterM

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]

filterM' _ [] = return []
filterM' p (x:xs)
  = do flag <- p x
       ys <- filterM' p xs
       if flag then return (x:ys) else return ys


-- Ex (9)  - foldLeftM

foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a

foldLeftM f b0 ls = step b0 ls
  where
    step b []         = return b
    step b (a:as)     = f b a >>= (step `flip` as)

io9 = foldLeftM (\a b -> putChar b >> return (b : a ++ [b])) [] "haskell" >>= \r -> putStrLn r
-- => haskelllleksahhaskell


-- Ex (10)  - foldRightM

foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b

foldRightM f b0 ls = step b0 ls where
    step b []         = return b
    step b (a:as)     = f a =<< step b as


io10 = foldRightM (\a b -> putChar a >> return (a:b)) [] (show [1,3..10]) >>= \r -> putStrLn r
-- => ]9,7,5,3,1[[1,3,5,7,9]


-- Ex (10)  - liftM

liftM1 :: Monad m => (a -> b) -> m a -> m b

-- compiles
liftM1 f m
  = do x <- m
       return (f x)

