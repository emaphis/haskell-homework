-- | Reworking homework

module Homework where

-- Homework 2

 {-
(2)fix the this function
 N = a 'div' length xs
     where
        a = 10
       xs = [1,2,3,4,5]
-}


n1 = a `div` length xs
     where
       a = 10
       xs = [1,2,3,4,5]


{-
(3) Show how the library function 'last' that selects
    the last element of a list can be defined using the
    functions introduced in the lecture
-}

last1 ::  [a] -> a
last1 xs = head (reverse xs)

--(4) Can you think of another possibel definition

last2   :: [a] -> a
last2 xs = head (drop (length xs - 1)  xs)

{-
(5) Similarly, show how the library function init
    the removes the last element from a list can be
    defined in two different ways.
-}

init1   :: [a] -> [a]
init1 xs = reverse (tail (reverse xs))

init2   :: [a] -> [a]
init2 xs = take (length xs - 1) xs

-- Homwork 2
{-
(1) What are the types of the following values:
['a','b','c'] :: [Char]
('a','b','c') :: (Char,Char,Char)
[(False,'0'),(True,'1')] :: [(Bool,Char)]
([False,True],['0','1']) :: ([Bool],[Char])
[tail,init,reverse]  :: [[a] -> [a]]

-}


--(2)- What are the type of the following functions

second :: [a] -> a
second xs  = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair' :: a -> b -> (a,b)
pair' x y   = (x,y)

double ::  Num a => a -> a
double x   = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice ::  (a->a) -> a -> a
twice f x  = f (f x)


-- Exercesis 3

{- (1) define safetail
     (a) conditional
     (b) quarded equation
     (c) pattern matiching

null :: [a] -> Bool can test of empty list
-}


{-(2) three possibel definiton for the logical
      operator (||) using attern matckig
-}

{- (3) redifine the following version of (&&) using
       conditonals rether than pattern

 True && True = True
 _    && _    = False

-}

{- (4) do the same for the following version
 True  && b = b
 False && _ = False
-}


-- Ex 4

{-(1) a triple (x,y,z) of positive intgers is call
   pytahgorean if x^2 + y^2 = z^2 using a comprehensions  define:

  pyths :: Int [(Int,Int,Int)]

  that mapts an ineger n ot all such tripples
  with components in [1..n]
  pyths 5 => [(2,4,5),(4,3,5)]
-}

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n],
                     y <- [1..n],
                     z <- [1..n],
                     x^2 + y^2 == z^2]


{- (2) a positive intger is perfect if it equals the of
       all of its factores excluding the number itself

   perfects :: Int -> [Int]
   perfects 500 [6,28,496]
-}

factors  :: Int -> [Int]
factors n =
  [x | x <- [1..n], n `mod` x == 0]


perfects :: Int -> [Int]
perfects n = [x | x <- [1..n],
                  sum (init (factors x)) == x]


-- Exercises 5

-- decide if all logical values in a list are True
and'       :: [Bool] -> Bool
and' []     = True
and' (x:xs) = (&&) x (and' xs)

-- concat a lisf of lists
concat'         :: [[a]] -> [a]
concat' []       = []
concat' (xs:xss) = (++) xs (concat' xss)


-- produce a list with n identical elements
replicate' :: Int -> a -> [a]
replicate' 0 _  = []
replicate' n i  = (:) i (replicate' (n-1) i)

-- Select the nth element of a list
(!!!) :: [a] -> Int -> a
(x:_)  !!! 0  = x
(_:xs) !!! n  = xs !!! (n-1)

-- Decide of a value is an element of a list
elem'         :: Eq a => a -> [a] -> Bool
elem' _ []     = False
elem' i (x:xs) | i==x      = True
               | otherwise = elem' i xs

--(4) merge two sorted lists
merge :: Ord a => [a] -> [a] -> [a]
merge xs  []   = xs
merge []  ys   = ys
merge (x:xs) (y:ys)
  | x<=y       = x : merge xs (y:ys)
  | otherwise  = y : merge (x:xs) ys

-- (5)  merge sort
halve :: [a] -> ([a], [a])
halve xs = splitAt len xs
  where len = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort []   = []
msort [x]  = [x]
msort xs  = merge (msort ys) (msort zs)
  where (ys,zs) = halve xs


-- Exercesis 7

--(1) what are higher-order functions that return functions
--    as results known as
-- curried fucntions

--(2) Express [f x | x <- xs, p x]
--   using map and filter

fn1,fn2 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
fn1 f p xs = [f x | x <- xs, p x]

fn2 f p xs =  map f (filter p xs)

--(2) define functions
all1,all2,all3 :: (a -> Bool) -> [a] -> Bool

all1 _ []     = True
all1 p (x:xs)
  | p x       = all1 p xs
  | otherwise = False

all2 p = and . map p

all3 p xs = foldr (&&) False (map p xs)


any1,any2,any3 :: (a -> Bool) -> [a] -> Bool

any1 _ []     = False
any1 p (x:xs)
  | p x       = True
  | otherwise = any1 p xs

any2 p = or . map p

any3 p xs = foldr (\x acc -> (p x) || acc) False xs

--any4 p xs = foldr (||) False (map p xs)

takeWhile1,takeWhile2 :: (a -> Bool) -> [a] -> [a]

takeWhile1 _ []  = []
takeWhile1 p (x:xs)
  | p x          = x : takeWhile1 p xs
  | otherwise    = []

takeWhile2 p = foldr (\a -> (if p a then (a:) else const [])) []

--(3) redifine map f and filter p using folder

map1,map2 :: (a -> b) -> [a] -> [b]

map1 _ [] = []
map1 f (x:xs) = f x : map f xs

map2 f = foldr  (\x xs -> f x : xs) []


filter1,filter2 :: (a -> Bool) -> [a] -> [a]

filter1 _ []  = []
filter1 p (x:xs)
  | p x        = x : filter1 p xs
  | otherwise  = filter1 p xs

filter2 p = foldr (\a as -> if p a then a:as else as) []
