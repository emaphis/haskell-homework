-- | Homework for Lecture 5 and Chapter 6
-- | Recursive functions

module Homework05 where

-- Ex 6.1   define exponentiation operator:

(^-)   ::(Num a, Integral b) => a -> b -> a
_ ^- 0  = 1
n ^- m  = n * (n ^- (m-1))

-- 2 ^ 3
-- 2 * (2^2)
-- 2 * (2 * (2^1))
-- 2 * (2 * (2 * (2^0)))
-- 2 * (2 * (2 *  1))
-- 8

-- Ex (0)
(^~),(^!),(^@),(^$),(^%),(^&)
 ::(Num a, Integral b) => a -> b -> a

m ^~ 0  = 0   -- bad - wrong base case
m ^~ n  = m * m ^~ (n-1)

m ^! 0  = 1   -- Good
m ^! n = m * m ^! (n-1)

-- m ^@ 0  = 1  -- bad - infinite loop
-- m ^@ n  = m * m ^@ n - 1

(^#) :: (Num a, Eq a) => a -> a -> a
m ^# 0  = 1  -- bad - wrong type
m ^# n  = n * n ^# (m-1)

m ^@ 0  = 1   -- Good
m ^@ n  = m * (^@) m (n-1)

m ^$ 0  = 1   -- Bad, Bad. Bad - infinite loop on ^1
m ^$ n  = m * m  * m ^$ (n-2)

m ^% 0  = 1   -- Bad
m ^% n  = (m*m) ^% (n-1)

m ^& 1  = m   -- Bad, doesn't handle base case right
m ^& n  = m * m ^& (n-1)

testExp :: (Num a, Eq a) => (a -> a -> a) -> Bool
testExp fn =
  3 `fn` 0  ==  1 &&
  3 `fn` 1  ==  3 &&
  3 `fn` 4  == 81


-- Ex 6.2  -- evaluate length, drop 3, and init

-- Ex (0)
-- length [1,2,3]
-- { applying length }
-- 1 + length [2,3]
-- { applying length }
-- 1 + (1 + length[3])
-- { applying length }
-- 1 + (1 + (1 + length[]))
-- { applying length }
-- 1 + (1 + (1 + 0))
-- { applying +}
-- 1 + (1 + 1)
-- { applying +}
-- 1 + 2
-- { applying +}
-- 3

-- Ex (2)
-- drop 3 [1,2,3,4,5]
-- { applying drop }
-- drop 2 [2,3,4,5]
-- { applying drop }
-- drop 1 [3,4,5]
-- { applying drop }
-- drop 0 [4,5]
-- { applying drop }
-- [4,5]

init'        :: [a] -> [a]
init' [_]     = []   -- dump the last
init' (x:xs)  = x : init xs

-- Ex (3)
-- init [1,2,3]
-- { applying int }
-- 1 : init [2:3]
-- { applying int }
-- 1 : 2 : init [3]
-- { applying int }
-- 1 : 2 : []
-- { list notation }
-- [1:2]


-- Ex 6.3  -- below

-- Ex (4)

and'       :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and' xs

and1,and2,and3,and4,and5,and6,and7,and8
   :: [Bool] -> Bool

-- Good
and1 []     = True
and1 (b:bs) = b && and1 bs

-- Good
and2 []     = True
and2 (b:bs) | b = and2 bs
            | otherwise = False

-- bad - wrong base case
and3 []     = False
and3 (b:bs) = b && and3 bs

-- bad - wrong base case
and4 []     = False
and4 (b:bs) = b || and4 bs

-- Good  - yeah, it actually works
and5 []     = True
and5 (b:bs) | b==False  = False
            | otherwise = and5 bs

-- bad  - || is wrong function
and6 []     = True
and6 (b:bs) = b || and6 bs

-- Good
and7 []     = True
and7 (b:bs) = and7 bs && b

-- bad
and8 []     = True
and8 (b:bs) | b         = b
            | otherwise = and8 bs

testAnd :: ([Bool] -> Bool) -> Bool
testAnd fn =
  fn []                      &&
  fn [True]                  &&
  not (fn [False])           &&
  fn [True, True]            &&
  not (fn [False, True])     &&
  not (fn [True,False,True]) &&
  fn [True,True,True]

-- Ex (5)
concat'       :: [[a]] -> [a]
concat' []      = []
concat' (xs:xss) = xs ++ concat' xss

concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 (xs:xss) = xs ++ concat2 xss


-- Ex (6)
replicate'    :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x


--- Ex (7)  - the third
(!!!)        :: [a] -> Int -> a
(x:_)  !!! 0  = x
(_:xs) !!! n  = xs !!! (n-1)


-- Ex (8)   -- the first
elem'         :: Eq a => a -> [a] -> Bool
elem' _ []     = False
elem' x (y:ys)
  | x==y       = True
  | otherwise  = elem' x ys


-- Ex (9) -- the fourth
-- Ex 6.4  -- merge two sorted lists
merge        :: Ord a => [a] -> [a] -> [a]

merge [] ys   = ys
merge xs []   = xs
merge (x:xs) (y:ys)
--  | x <= y    = x : merge xs (y:ys)
--  | otherwise = y : merge (x:xs) ys
 = if x <= y then x : merge xs (y:ys) else y : merge (x:xs) ys

-- Ex(10)  -- the second
-- Ex 6.5  -- merge sort
halve :: [a] -> ([a],[a])
halve xs = (take ln xs, drop ln xs)
  where ln = length xs `div` 2

msort    :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  =  merge (msort ys) (msort zs)
  where (ys,zs) = halve xs


-- Ex 6.6  -- define some library functions

sum'       :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = (+) x (sum' xs)

take'         :: Int -> [a] -> [a]
take' 0 _      = []
take' n (x:xs) = (:) x (take' (n-1) xs)

last'        :: [a]  -> a
last' [x]     = x
last' (_:xs)  = last' xs

