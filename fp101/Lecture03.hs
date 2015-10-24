-- | Lecture03  -- Defining Functions
-- | Book Chapter 4

module Lecture03 where


-- 4.1   New from old
-- the usuall practice in Haskell is to create new functions
-- by composing preexisting functions


-- 4.2   Conditional expressions
abs1  :: Int -> Int
abs1 n = if n >= 0 then n else -n

-- Conditional expressions can be nested:
signum1   :: Int -> Int
signum1 n  = if n < 0 then -1 else
               if n == 0 then 0 else 1
-- conditionals must have an else part


-- 4.3   Guarded equations
abs2  :: Int -> Int
abs2 n | n >= 0    = n
       | otherwise = -n

signum2  :: Int -> Int
signum2 n | n < 0      = -1
          | n == 0     = 0
          | otherwise  = 1


-- 4.4   Pattern matching

not1      :: Bool -> Bool
not1 False = True
not1 True  = False

(&&-)   :: Bool -> Bool -> Bool
True  &&- True  = True
True  &&- False = False
False &&- True  = False
False &&- False = False

-- or more concisely

(&&*)   :: Bool -> Bool -> Bool
True &&* True  = True
_    &&* _     = False

(&&^)   :: Bool -> Bool -> Bool
True  &&^ b  = b
False &&^ _  = False

(&&#)   :: Bool -> Bool -> Bool
b &&# c | b==c       = b
        | otherwise  = False

-- Tuple patterns
-- must match tuples of the same arity

fst'       :: (a,b) -> a
fst' (x,_)  = x

snd'       :: (a,b) -> b
snd' (_,y)  = y


-- List pattern matching (x:xs)

-- test if a list contains three characters and begins with 'a'
test          :: [Char] -> Bool
test ['a',_,_] = True
test _         = False

-- test if a string starts with 'a'
test2         :: [Char] -> Bool
test2 ('a':_)  = True
test2 _        = False


-- primary list functions
null'        :: [a] -> Bool
null' []      = True
null' (_:_)   = False

head'        :: [a] -> a
head' (x:_)   = x

tail'        :: [a] -> [a]
tail' (_:xs)  = xs


-- Integer patterns  -- not in modern Haskell
-- form n+k where n is an integer and k>0 is a constant

-- map 0 to itself and any other nonzero number to a predissor
-- kinda subtraction
--pred       :: Int -> Int
--pred 0      = 0
--pred (n + 1)  = n


-- 4.5  Lambda expressions
-- functions can be defined without naming

int4 = (\x -> x + x) 2  -- => 4


double :: Integer -> Integer
double = \x -> x + x


-- currying made explicit
add'    :: Int -> Int -> Int
add' x y = x+y

add     :: Int -> Int -> Int
add = \x -> (\y -> x+y)

int15 = add 10 5

-- using lambda expressions when defining functions
-- that return functions as results
const'     :: a -> b -> a
const' k _  = k

const      :: a -> (b -> a)
const k = \_ -> k


-- lambda expressions can be used ot avoid naming functions
-- that are used only once

odds1  :: Int -> [Int]
odds1 n = map f [0..n-1]
          where
            f x = x*2 + 1

-- simplify to:
odds2  :: Int -> [Int]
odds2 n = map (\x -> x*2 + 1) [0..n-1]


-- 4.6  Sections

-- (@)   = \x -> (\y -> x@y)
-- (x@)  = \y -> x @ y
-- (@y)  = \x -> x @ y

-- (1+)  -- successor function
-- (1/)  -- reciprocation function
-- (*2)  -- doubling function
-- (/2)  -- halving function
