-- | Chapter 9 - Playing the gameL I/O in Haskell

module Chapter8 where

-- Scoring the moves
import Data.Time
import System.Locale
import System.IO.Unsafe
import System.IO
import Test.QuickCheck
import Test.HUnit



data Move = Rock | Paper | Scissors
          deriving (Eq)

type Tournament = ([Move],[Move])

-- show in an abbreviated form.
instance Show Move where
  show Rock = "r"
  show Paper = "p"
  show Scissors = "s"

-- for Quickcheck to work ovet the Move type
instance Arbitrary Move where
  arbitrary = elements [Rock, Paper, Scissors]

-- example tournament
ex1 :: Tournament
ex1 = ([Rock,Rock,Paper],[Scissors,Paper,Rock])


-- convert from 0,1,2 to a Move
convertToMove :: Integer -> Move
convertToMove 0 = Rock
convertToMove 1 = Paper
convertToMove 2 = Scissors

-- convert a Character to the corresponding Move
convertMove :: Char -> Move
convertMove 'r' = Rock
convertMove 'R' = Rock
convertMove 'p' = Paper
convertMove 'P' = Paper
convertMove 's' = Scissors
convertMove 'S' = Scissors

-- Exercises

-- Ex 8.1
-- Outcome of a play
--   +1 for first player wins
--   -1 for second player wins
--    0 for a draw

outcome  :: Move -> Move -> Integer
outcome Rock Scissors     = 1
outcome Paper Rock        = 1
outcome Scissors Paper    = 1
outcome Rock Rock         = 0
outcome Paper Paper       = 0
outcome Scissors Scissors = 0
outcome _ _ = -1


-- Ex 8.2

tournamentOutcome :: Tournament -> Integer
tournamentOutcome ([],[])   = 0
tournamentOutcome ((x:xs),(y:ys)) =
  outcome x y + tournamentOutcome (xs, ys)
tournamentOutcome (_, _) = error "Bad tournament"

test_tournamentOutcome = TestList [
  TestCase (assertEqual "base case" 0 (tournamentOutcome ([],[]))),
  TestCase (assertEqual "ex1" 1 (tournamentOutcome ex1)),
  TestCase (assertEqual "one match" 1 (tournamentOutcome ([Rock],[Scissors])))
  ]


beat,lose :: Move -> Move

-- what beats
beat Rock     = Paper
beat Paper    = Scissors
beat Scissors = Rock

-- what loses to
lose Rock     = Scissors
lose Paper    = Rock
lose Scissors = Paper


-- Strategies

type Strategy = [Move] -> Move
-- a move may be dependent on all previous moves

-- Constant strategy - always play the same thing
rock, paper, scissors :: Strategy

rock _     = Rock
paper _    = Paper
scissors _ = Scissors

-- Cycle through the possiblities
cycle' :: Strategy

cycle' moves
  = case (length moves) `rem` 3 of
      0 -> Rock
      1 -> Paper
      2 -> Scissors

-- Random choice of Move
randomStrategy :: Strategy
randomStrategy _ = convertToMove $ randInt 3


-- Echo the last move

echo :: Strategy

echo (latest:rest) = latest
echo []            = Rock


-- Exercises  - Strategies

-- Ex 8.3
-- move which would beat opponents last move
won :: Strategy
won mv = beat (head mv)

-- move which would a lost to oppents last move
lost :: Strategy
lost mv = lose (head mv)


-- Ex 8.4  - play against opponents last two moves or randon
lastTwo :: Strategy
lastTwo (x:y:_)
  | x==y   = beat x
--  | otherwise = randomStrategy zs
lastTwo xs = randomStrategy xs


-- Ex 8.5 - frequency strategy

-- calculate the frequency of moves in a [Move]
freq :: [Move] -> (Int,Int,Int)
freq mv = (a,b,c)
  where a = count Rock mv
        b = count Paper mv
        c = count Scissors mv

-- count the number of Move in [Move]
count :: Move -> [Move] -> Int
count _ [] = 0
count mv (m:ms)
  | mv==m     = 1 + count mv ms
  | otherwise = count mv ms

-- pick the highest frequency Move
highest :: (Int,Int,Int) -> Move
highest (rc,pp,sc)
  | rc >= pp && rc >= sc = Rock
  | pp >= rc && pp >= sc = Paper
  | otherwise            = Scissors

highFreq :: Strategy
highFreq mv = highest (freq mv)

-- Ex 8.6   TODO:

-- Ex 8.7 - alternate between two given Strategies
alternate :: Strategy -> Strategy -> Strategy
alternate st1 st2 mv =
  if odd (length mv) then st1 mv else st2 mv

test1 :: Strategy
test1 = alternate rock paper



-- Generate a random integer within the IO monad

randomInt :: Integer -> IO Integer
randomInt n =
  do time <- getCurrentTime
     return ( (`rem` n) $ read $ take 6 $ formatTime defaultTimeLocale "%q" time)

-- Extract the random number from the IO monad
randInt :: Integer -> Integer
randInt = unsafePerformIO . randomInt



-- 8.3  The basics of Input/output

-- type IO a - IO actions of type a or IO programs of type a


-- Reading input
-- getLine :: IO String
-- getChat :: IO Char


-- The one-element type: ()
-- IO ()  - like output and returning a void.
-- only significant in side effects, return type is irrelivant.


-- The Main module and the main program:

-- main :: IO t  for some type t

-- main :: IO ()


-- Writing Strings:

-- putStr :: String -> IO ()

-- a helloworld program

helloWorld :: IO ()
helloWorld = putStr "Hello, World!"

-- using putStr write a function to write a line of output

putStrLn' :: String -> IO ()
putStrLn' = putStr . (++ "\n")


-- Writing values in general:

-- show :: Show a => a -> String

print' :: Show a => a -> IO ()
print' = putStrLn' . show


-- Returning a value:

-- return :: a -> IO a

-- return x -- do no IO but return an 'x'

-- only perform an IO action if a condition holds:

-- if condition
--   then action
--   else return ()

-- Running an I/O program
-- wrap any expression of any type in a print
-- expression to create an IO a object.


-- 8.4 'do' notation
-- used to sequence I/O programs
-- names values returned by IO actions


-- Sequencing I/O actions

-- Examples:

-- 1.
putStrLn'' :: String -> IO ()
putStrLn'' str = do putStr str
                    putStr "\n"

-- 2. print something four times
put4Times :: String -> IO ()
put4Times str
  = do putStrLn str
       putStrLn str
       putStrLn str
       putStrLn str

-- 3. read two lines:
read2Lines :: IO ()
read2Lines
  = do getLine
       getLine
       putStrLn "Two lines read."


-- Capturing the values read:

-- Examples:

-- 4. read a line then write a line
getNput :: IO ()
getNput = do line <- getLine
             putStrLn line

-- 5. reverse two lines:
reverse2Lines :: IO ()
reverse2Lines
  = do line1 <- getLine
       line2 <- getLine
       putStrLn (reverse line2)
       putStrLn (reverse line1)


-- local definitions in a do expression
-- var <- getlines
-- let var2 = fun var

-- Example

-- 6. reverse two line revisited:
reverse2Lines' :: IO ()
reverse2Lines'
  = do line1 <- getLine
       line2 <- getLine
       let rev1 = reverse line1
       let rev2 = reverse line2
       putStrLn rev2
       putStrLn rev1


-- Reading values in general

-- the Read class:
-- read :: REad a => String -> a

-- Example:

-- 7. read an integer from IO

getInt :: IO Integer
getInt = do line <- getLine
            return (read line :: Integer)


-- Exercises

-- Ex 8.10 -- test palendrome

palendrome :: String -> Bool
palendrome str = str == reverse str

testPalendrome :: IO ()
testPalendrome
  = do putStrLn "Enter a string so see if itss a palendrome"
       line <- getLine
       let out = if palendrome line
                 then "input is a palendrome"
                 else "input is not a palendrome"
       putStrLn out

-- Ex 8.11 - get two integers from input and add them
addTwo :: IO ()
addTwo = do putStrLn "Enter and integer:"
            int1 <- getInt
            putStrLn "Enter another integer:"
            int2 <- getInt
            let str = "the numbers " ++
                       show int1 ++ " and " ++
                       show int2 ++  " added, equal " ++
                       show (int1+int2)
            putStrLn  str

-- Ex 8.12  put a string N times
putNtimes :: Integer -> String -> IO ()
putNtimes n str
  = do putStrLn str
       if n>0
         then putNtimes (n-1) str
         else putStrLn "end."

-- Ex 8.13 -- addNIntegers

sumNintegers :: IO ()
sumNintegers
  = do putStr "Number Ints to add then enter one per line"
       n <- getInt
       m <- sumInts n
       let str = "The sum is: " ++ show m
       print str
         where sumInts n
                 = do if n <= 0
                        then return 0
                        else do putStr "Enter an integer: "
                                i   <- getInt
                                acc <- sumInts (n-1)
                                return (i + acc)


-- 8.5 - Loops and recursion

-- Examples

-- 8. - copy
copy :: IO ()
copy =
  do line <- getLine
     putStrLn line
     copy

-- 9. copyN
copyN :: Integer -> IO ()
copyN n =
  if n <= 0
  then return ()
  else do line <- getLine
          putStrLn line
          copyN (n-1)

-- 10. copy until condition on data:
copyEmpty :: IO ()
copyEmpty =
  do line <- getLine
     if line == ""
       then return ()
       else do putStrLn line
               copyEmpty

-- 11. count copied lines
copyCount :: Integer -> IO ()
copyCount n =
  do line <- getLine
     if line == ""
       then putStrLn (show n ++ " lines copied.")
       else do putStrLn line
               copyCount (n+1)


-- Exercises

-- 8.14 - word count
wc :: IO ()
wc = wcLoop 0 0 0
  where wcLoop lcnt wcnt ccnt  =
          do line <- getLine
             if line == ""
               then do putStrLn ("line count: " ++ show lcnt)
                       putStrLn ("word count: " ++ show wcnt)
                       putStrLn ("char count: " ++ show ccnt)
               else
                 wcLoop (lcnt + 1)
                        (wcnt + length (words line))
                        (ccnt + length (concat (words line)))


-- 8.15 - interactive palendrome

-- check for palendrome:
palindrome :: String -> Bool
palindrome str = 

