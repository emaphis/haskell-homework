-- | Programming with Lists

module Chapter6 where

--import Test.HUnit
import Prelude hiding (id)
--import Test.QuickCheck

-- 6.1  Generic functions: polymorphism

id :: a -> a
id x = x

mystery :: (Bool, a) -> Char
mystery (x,_) = if x then 'c' else 'd'


-- Polymorphism and overloading

-- polymorphic - same definition over different types
-- overloaded  - different definition over different types


-- Exercises

-- Ex 6.1 - give the most general type for:

snd' :: (a, b) -> b
snd' (_, y) = y

sing :: a -> [a]
sing x = [x]


-- Ex 6.2 -- [[a]] -> [[a]] - isn't the most general type, it is
-- constrained ot by a list of list of some type a.  A type 'a'
-- hold more types including lists of lists


-- Ex 6.3 - what is the most general type of shift.

--shift :: ((Integer,Integer),Integer) -> (Integer,(Integer,Integer))
shift :: ((a,b),c) -> (a,(b,c))
shift ((x,y),z) = (x,(y,z))


-- 6.2 - Haskell list funtions in the Prelude


-- 6.3 Finding your way around the Haskel libraries

