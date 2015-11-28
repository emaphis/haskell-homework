-- | Lab 4

module Lab4 where

------------------------------------------------------------------------------------------------------------------------------
-- RECURSIVE FUNCTIONS
------------------------------------------------------------------------------------------------------------------------------

import Data.Char

-- ===================================
-- Ex. 0
-- ===================================

triangle :: Integer -> Integer
triangle 0 = 0
triangle n = n + triangle (n-1)

-- triangle 500 => 125250

-- ===================================
-- Ex. 1
-- ===================================

count :: Eq a => a -> [a] -> Int
count _ []     = 0
count n (x:xs)
  | x==n       = 1 + count n xs
  | otherwise  = count n xs

xs = [1,2,35,2,3,4,8,2,9,0,5,2,8,4,9,1,9,7,3,9,2,0,5,2,7,6,92,8,3,6,1,9,2,4,8,7,1,2,8,0,4,5,2,3,6,2,3,9,8,4,7,1,4,0,1,8,4,1,2,4,56,7,2,98,3,5,28,4,0,12,4,6,8,1,9,4,8,62,3,71,0,3,8,10,2,4,7,12,9,0,3,47,1,0,23,4,8,1,20,5,7,29,3,5,68,23,5,6,3,4,98,1,0,2,3,8,1]
ys = map (\x -> ((x + 1) * 3) ^ 3 - 7) xs

poem = [ "Three Types for the Lisp-kings under the parentheses,"
       , "Seven for the Web-lords in their halls of XML,"
       , "Nine for C Developers doomed to segfault,"
       , "One for the Dark Lord on his dark throne"
       , "In the Land of Haskell where the Monads lie."
       , "One Type to rule them all, One Type to find them,"
       , "One Type to bring them all and in the Lambda >>= them"
       , "In the Land of Haskell where the Monads lie."
       ]

-- ===================================
-- Ex. 2
-- ===================================

euclid :: (Int,  Int) -> Int
euclid (x, y)
  | x==y        = x
  | x>y         = euclid (x-y, y)
  | otherwise   = euclid (x, y-x)

-- euclid (a, b)
--   | b==0      = a
--   | otherwise = euclid (b, a `mod` b)

-- euclid (13404, 8832) --=> 12

-- ===================================
-- Ex. 3
-- ===================================

funkyMap :: (a -> b) -> (a -> b) -> [a] -> [b]
funkyMap _ _ []   = []
funkyMap f _ [x]  = [f x]
funkyMap f g (x:y:zs)  = f x : g y : funkyMap f g zs

-- sum $ funkyMap (+10) (+100) ys
-- => 112319712
-- sum $ funkyMap (\c -> if c == 'e' then 1 else 0) ord (poem >>= id)
-- =>16805


-- Ex (8)
fn8 :: a -> a
fn8 = \ a -> a


-- Ex (9)

lst9 :: [a]
lst9 = [undefined]


-- Ex (10)
thng10 :: (Bool, Bool)
thng10 = (True,(False))


-- Ex (11)
f11 :: t42 -> t4711 -> (t4711, t42)
f11 a = \b -> (b, a)


-- Ex (12)
f12 :: (a -> ([(a -> a)] -> a))
f12 = foldr id


-- Ex 13
f13 ::  (a -> (c -> (b -> c)) -> c -> (b -> c)) -> [a] -> c -> (b -> c)
f13 = flip foldr const


-- Ex 14
dup :: a -> (a, a)
dup a = (a, a)

f14 :: (a) -> ((((a), (a)), ((a), (a))), (((a), (a)), ((a), (a))))
f14 = dup . dup . dup

-- Ex 15
h ::  ((a -> b) -> a) -> ((a -> b) -> b)
h g f = (f . g) $ f


-- Ex 18
fix :: (a -> a) -> a
fix = h fix

-- Ex 17
-- Fix is Higher order, polymorphic, recursive


-- Ex 18  - wow this is factorial.
f18 ::  (Eq a, Num a) => (a -> a) -> a -> a
f18 = \f n -> if (n == 0) then 1 else n * f (n - 1)

-- Ex 19

k :: Integer -> Integer
k = fix $ f18

num19 :: Integer
num19 = k 42
