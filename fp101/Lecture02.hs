-- | Lecture02  -- Types and Classes
-- | Book Chapter 3

module Lecture02 where
import Data.Char


-- 2.1  Basic Concepts
-- A type is name for a collectin of related values.
-- Type errors
-- e :: t - e has a type of t

bool1 = False :: Bool
bool2 = True  :: Bool
fun1  = not   :: Bool -> Bool

-- types can refer to expressions that haven't been
-- evaluated yet

bool3 = not False        :: Bool
bool4 = not True         :: Bool
bool5 = not (not False)  :: Bool

-- Type inference:

-- f :: A -> B,  e::A  =>  f e :: B


-- 2.2  Basic Types
-- Bool, Char, String, Int, Integer, Float


-- 2.3  List Types
-- A sequence of values with the same type, but different lengths

list1 = [False,True,False] :: [Bool]
list2 = ['a', 'b','c','d'] :: [Char] -- :: String
-- [t] is the type of lists with elements of type t.

-- list can be any length
list3 = [False,True] :: [Bool]
list4 = [False,True,False] :: [Bool]

-- list of lists:
list5 = [['a'],['b','c']] :: [[Char]]


-- 2.4  Tuple Types
-- A sequence of values with the same length but different types

tuple0 = ()                :: ()
tuple1 = (False,True)      :: (Bool,Bool)
tuple2 = (False,'a',True)  :: (Bool,Char,Bool)
tuplea = ("Yen",True,'a')  :: (String,Bool,Char)

-- The type of a tuple encode its size:
tuple3 = (False,True) :: (Bool, Bool)   -- two
tuple4 = (False,True,False) :: (Bool,Bool,Bool) -- three

-- The type of the components is unrestricted:
tuple5 = ('a',(False,'b'))  :: (Char,(Bool,Char))
tuple6 = (True,['a','b'])   :: (Bool,[Char])


-- 2.5  Function Types
-- a mapping from values of one type to values of another type:

-- not :: Bool -> Bool
-- isDigit :: Char -> Bool

-- Domain -> Range

add      :: (Int,Int) -> Int
add (x,y) = x+y

zeroto  :: Int -> [Int]
zeroto n = [0..n]

-- 2.6  Curried Functions
-- functions with multiple arguments are also possible by
-- returning functions as results.

add'   :: Int -> (Int -> Int)
add' x y = x+y

-- functions with more than two arguments can be curried
-- by returning neted functions:
mult    :: Int -> (Int -> (Int -> Int))
mult x y z = x*y*z

-- partial application to create specialized fucntions

add1 :: Int -> Int
add1 = add' 1

take5 :: [a] -> [a]
take5 = take 5

-- The arrow -> associates to the right:
--    Int -> Int -> Int -> Int
-- means:
--    Int -> (Int -> (Int -> Int))

-- Function application associates to the left:
--    mult x y z
-- means:
--    ((mult x) y) z


-- 2.7  Polymorphic Types
-- a function is polymorphic if it's type contains one
-- or more type variables

-- length :: [a] -> Int   -- any type of list
int4 = length [1,3,5,7]
int2 = length ["yes", "no"]
int3 = length [isDigit, isLower, isUpper]

-- some functions from the standard prelude:
-- fst   :: (a,b) -> a
-- head  :: [a] -> a
-- take  :: Int -> [a] -> [a]
-- zip   :: [a] -> [a] -> [(a,b)]
-- id    :: a -> a


-- 2.8  Overloaded Types
-- an overloaded function if it's type contains one or
-- more class constraints

-- sum :: Num a => [a] -> a

-- so:
int6 = sum [1,2,3]           :: Int
float66 = sum [1.1,2.2,3.3]  :: Float
-- no no - sum['a','b','c']


-- 2.9  Basic Classes

-- Haskell type classes:

-- Eq   - Equality types
-- (==) :: Eq a => a -> a -> Bool
-- (/=) :: Eq a => a -> a -> Bool

-- Bool, Char, String, Int, Integer, Float are part of Eq
-- list and tuple types

bool6 = False == False
bool7 = 'a' == 'a'
bool8 = [1,2] == [1,2,3]

-- Ord  - Oredered types
-- (<)  :: Ord a => a -> a -> Bool
-- (<=) :: Ord a => a -> a -> Bool
-- (>)  :: Ord a => a -> a -> Bool
-- (=<) :: Ord a => a -> a -> Bool
-- min  :: a -> a -> a
-- max  :: a -> a -> a

-- Bool , Char, String, Int, Integer, Float are part of Ord
-- also list and tuple types

bool9  = False < True
bool10 = min 'a' 'b'  -- 'a'
bool11 = "elegant" < "elephant"
bool12 = [1,2,3] < [1,2]
bool13 = ('a',2) < ('b',1)


-- Show - showable types
-- types that can be converted to strings

-- show  :: Show a => a -> String
str1 = show False       -- "False"
str2 = show 'a'         -- "'a'"
str3 = show 123         -- "123"
str4 = show [1,2,3]     -- "[1,2,3]"
str5 = show ('a',False) -- "('a',False)"


-- Read - readable types
-- types that can be converted from Strings

-- read :: Read a => String -> a

-- Bool, Char, String, Int, Integer, Float
-- also list and tuple types with readable components



-- Num  - Numeric types
-- also Eq and Show

-- Int, Integer, Float

-- (+)    :: Num a => a -> a -> a
-- (-)    :: Num a => a -> a -> a
-- (*)    :: Num a => a -> a -> a
-- negate :: Num a => a -> a
-- abs    :: Num a => a -> a
-- signum :: Num a => a ->a


-- Integral - inegral types

-- Int, Integer

-- div :: Integral a => a -> a -> a
-- mod :: Integral a => a -> a -> a

--Fractional - fractional types

-- Float

-- (/)   :: Fractional a => a -> a -> a
-- recip :: Fractional a => a -> a
--
