-- | Lecture 8 -- Interactive Programs
-- | Chapter 9

module Lecture08 where
import System.IO


-- 9.1  Interaction



-- 9.2  The input/ouput type

-- type IO = World -> World

-- type IO a = World -> (a,World)

-- IO Char

-- IO () -- side effects without a return value


-- 9.3  Basic action

-- reads a character from the keyboard, echoes it
-- then returns it

-- getChar :: IO Char

-- writes a character to the screen, and return
-- no value

-- putChar :: Char -> IO ()


-- return a the result value 'v' without preforming
-- and interaction

-- return   :: a -> IO a
-- return v  = \ world -> (v,world)


-- getLine :: IO String



-- 9.4  Sequencing - one action after another

-- (>>=)   :: IO a -> (a -> IO b) -> IO b
-- f >>= g  = \ world -> case f world of
--                            (v,world') -> g v world'


--      T <- IO T
-- do { x <- E}

--     Char <- IO Char
-- do { c   <- getChar }

a, a' :: IO (Char,Char)
a  = do x <- getChar
        getChar
        y <- getChar
        return (x,y)

a' = getChar >>= (\ x ->
      getChar >>= (\ _ ->
        getChar >>= (\ y ->
          return (x,y))))



-- 9.5  Derived primitives

-- get a string frome the screen.
getLine' :: IO String
getLine'  = do x <- getChar
               if x == '\n' then
                  return []
                 else
                   do xs <- getLine
                      return (x:xs)

-- write a string to the screen
putStr'   :: String -> IO ()
putStr' []     = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs

--putStr'' str = foldr ^^ (return ()) str
--  where  x ^^ p = putChar x >>= \ _ -> p

-- Write a string an move to a new line
putStrLn'     :: String -> IO ()
putStrLn' xs   = do putStr' xs
                    putChar '\n'

-- Example
-- a function that promps for a string to be entered
-- then outputs it's length
strlen   :: IO ()
strlen    = do putStr "Enter a string: "
               xs <- getLine
               putStr "The string has "
               putStr (show (length xs))
               putStrLn " Characters"

-- some more primatives
beep  :: IO ()
beep   = putStr "\BEL"

cls   :: IO ()
cls    = putStr "\ESC[2J"

type Pos = (Int,Int)

goto       :: Pos -> IO ()
goto (x,y)  = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat      :: Pos -> String -> IO ()
writeat p xs  = do goto p
                   putStr xs

seqn         :: [IO a] -> IO ()
seqn []       = return ()
seqn (a:as)   = do a
                   seqn as

putStr1   :: String -> IO ()
putStr1 xs = seqn [putChar x | x <- xs]


-- A calculator

box :: [String]
box  = ["+---------------+",
        "|               |",
        "+---+---+---+---+",
        "| q | c | d | = |",
        "+---+---+---+---+",
        "| 1 | 2 | 3 | + |",
        "+---+---+---+---+",
        "| 4 | 5 | 6 | - |",
        "+---+---+---+---+",
        "| 7 | 8 | 9 | * |",
        "+---+---+---+---+",
        "| 0 | ( | ) | / |",
        "+---+---+---+---+"]

buttons  :: String
buttons   = standard ++ extra
  where standard = "gcd=123+456-789*9()/"
        extra = "QCD \ESC\BS\DEL\n"

showbox  :: IO ()
showbox   =
  seqn [writeat (1,y) xs | (y,xs) <- zip [1..13] box]



-- Hangman
hangman :: IO ()
hangman  = do putStrLn "Think of a word"
              word <- sgetLine
              putStrLn "Try to guess it"
              guess word


sgetLine  :: IO String
sgetLine   = do x <- getCh
                if x == '\n' then
                  do putChar x
                     return []
                  else
                  do putChar '_'
                     xs <- sgetLine
                     return (x:xs)


getCh   :: IO Char
getCh    = do hSetEcho stdin False
              c <- getChar
              hSetEcho stdin True
              return c


guess     :: String -> IO ()
guess word = do putStr "> "
                xs <- getLine
                if xs == word then
                  putStrLn "You got it!"
                  else
                  do putStrLn (diff word xs)
                     guess word

diff      :: String -> String -> String
diff xs ys = [if elem x ys then x else '_' | x <- xs]

-- 9.6  Calculatorn



-- 9.8  Game of life

luff :: (a -> b) -> [a] -> [b]
luff f xs = concat (map (\x -> [f x]) xs)


all' :: (a -> Bool) -> [a] -> Bool
all' p = not . any (not . p)
