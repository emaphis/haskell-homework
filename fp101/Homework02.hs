-- | Homework for Lecture 2 and Chapter 3
-- | Types and Classes

module Homework02 where

-- Ex (0,1,2,3)
-- 3.1 - What are the types of the following values?

lst1 =  ['a','b','c']              :: [Char]
tup2 =  ('a','b','c')              :: (Char,Char,Char)
lst3 =  [(False,'0'), (True,'1')]  :: [(Bool,Char)]
tup4 =  ([False,True],['0','1'])   :: ([Bool],[Char])
lst5 =  [tail,init,reverse]        :: [[a]->[a]]


-- Ex (5,6,7,8,9,10)
-- 3.2  - What are the types of the following functions?

second       :: [a] -> a
second xs     = head (tail xs)

swap         :: (a,b) -> (b,a)
swap (x,y)    = (y,x)

pair         :: a -> b -> (a,b)
pair x y      = (x,y)

double       :: Num a => a -> a
double x      = x * 2

palindrome   :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice        :: (a -> a) -> a -> a
twice f x     = f (f x)


-- 3.3  - Check your work in hugs:  Ok.


-- Ex (11)
-- 3.4  - Functions even if they have the same out put may not
--      teminate, so there can be no general test for equality.
-- Infeasible in general; only feasible for some functions.

-- Ex (12)

lst6 = [[]]          :: [[t]]
lst7 = [(1,2),(3,4)] :: (Num t1, Num t) => [(t, t1)]
lst8 = [[1,2],[3,4]] :: Num t => [[t]]
-- ['1',['2','3']]  -- invalid
lst9 = [(+),(-),(*)] :: Num a => [a -> a -> a]


-- Ex (13)  -- [1,[2,3],4]  is invalid

-- Ex (14)   - has type

lst10 = ["False", "True"]       :: [String]


-- Ex (15)  - has type:
lst11 = ([False, True], False)  :: ([Bool], Bool)


-- Ex (16)  - has type:

tup12 = ("1,2","3,4")           :: (String,String)


-- Ex (17)  - has type:

lst13 = [(1,True),(0,False)]    :: [(Int, Bool)]


-- Ex (18)  - has type:

f    :: [a] -> [a]
f xs  = take 3 (reverse xs)


-- Ex (19)  - the type means:

-- a -> b -> c -> d
-- a -> (b -> (c -> d))


-- Ex (20)  which contains an error:

--  [1,2,3] ++ 4
