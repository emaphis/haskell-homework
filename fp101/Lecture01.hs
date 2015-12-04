-- | Lecture 1 - First Steps

module Lecture01 where

-- Part 1

-- Karate Kid: Wax on, wax off.
-- Using GHC on homework is not cheeting, the compiler is there to help

-- 2.1  The Hugs System


-- 2.2  The Standard Prelude

-- Lists:
int1  = head [1,2,3,4,5] == 1
lst2  = tail [1,2,3,4,5] == [2,3,4,5]
int3  = [1,2,3,4,5] !! 2 == 3  --nth element
lst4  = take 3 [1,2,3,4,5]  == [1,2,3]
lst5  = drop 3 [1,2,3,4,5]  == [4,5]
--  xs == take n xs ++ drop n xs

int6  = length [1,2,3,4,5]  == 5
int7  = sum [1,2,3,4,5]     == 15
int8  = product [1,2,3,4,5] == 120
lst9  = [1,2,3] ++ [4,5] == [1,2,3,4,5]  -- append
lst10 = reverse [1,2,3,4,5] == [5,4,3,2,1]


-- 2.3  Function Application

-- f(a,b) + c d  - mathamatics
-- f a b + c * d -- Haskell
-- function application binds higher:
-- f a + b  => (f a) + b
-- Math       Haskell
-- f(x)       f x
-- f(x,y)     f x y
-- f(g(x))    f (g x)
-- f(x,g(y))  f x (g y)
-- f(x)g(y)   f x * g y


-- 2.4 - scripts

double :: Int -> Int
double x    = x + x

quadruple :: Int -> Int
quadruple x = double (double x)

-- (f . g) x = f (g x)

int10 = quadruple 10 == 40
lst11 = take (double 2) [1,2,3,4,5,6]
         == [1,2,3,4]


factorial n = product [1..n]

average ns  = sum ns `div` length ns

-- `` back quotes not '' forward quotes
-- x `f` y is just sytactic sugar for f x y.

-- function names begin with lower case letters:
-- myFun fun1 arg_2 x'

-- lists usually have an 's' suffix on their name:
-- xs ns nss

-- Layout rule - Haskell columns are significant.
-- but can use { ; ; } for explicit grouping

-- Useful GHCi Commands
-- :load name    load script name
-- :reload       reload current script
-- :edit name    edit script name
-- :edit         edit current script
-- :type expr    show type of expr
-- :?            help - show all commands
-- :quit         quit GHCi
