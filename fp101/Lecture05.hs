-- | Lecture 5 -- Recursive Functions
-- | Book Chapter 6

module Lecture05 where


-- 6.1  Basic concepts
-- tail call eliminations
factorial  :: Int -> Int
factorial n = product [1..n]

-- factorial 4
-- product [1..4]
-- product [1,2,3,4]
--

factorial'  :: Int -> Int
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

-- factorial' 3
-- 3 * factorial' 2
-- 3 * (2 * factorial 1)
-- 3 * (2 * (1 * factorial 0))
-- 3 * (2 * (1 * 1))
-- 3 * (2 * 1)
-- 3 * 2
-- 6

-- Some functions are simpler to define using recursion
-- Some functions are naturally defined in terms of themselves
-- properties can be proved by induction


-- 6.2  Recursion on lists
-- structural recursion

product'       :: Num a => [a] -> a
product' []     = 1
product' (n:ns) = n * product' ns

-- product [2,3,4]
-- 2 * product [3,4]
-- 2 * (3 * product [4])
-- 2 * (3 * (4 * product []))
-- 2 * (3 * (4 * 1))
-- 2 * (3 * 4)
-- 2 * 12
-- 24

-- same pattern as product
length'       :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

-- length [1,2,3]
-- 1 + length [2,3]
-- 1 + (1 + length[3])
-- 1 + (1 + (1 + length[]))
-- 1 + (1 + (1 + 0))
-- 3

reverse'       :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse xs ++ [x]

-- reverse [1,2,3]
-- reverse [2,3] ++ [1]
-- (reverse [3] ++ [2]) ++ [1]
-- ((reverse [] ++ [3]) ++ [2]) ++ [1]
-- (([] ++ [3]) ++ [2]) ++ [1]
-- [3,2,1]

-- insert an item into a sorted list
insert'         :: Ord a => a -> [a] -> [a]
insert' x []     = [x]
insert' x (y:ys)
     | x <= y    = x:y:ys
     | otherwise = y : insert' x ys


-- insertion sort
isort       :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert' x (isort xs)


-- 6.3  Multiple arguments
-- functions with more than one argument can
-- be defined using recursion

zip'              :: [a] -> [b] -> [(a,b)]
zip' [] _          = []
zip' _  []         = []
zip' (x:xs) (y:ys) = (x,y) : zip xs ys


drop'         :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' _ []     = []
drop' n (_:xs) = drop (n-1) xs


(+++)         :: [a] -> [a] -> [a]
[]     +++ ys  = ys
(x:xs) +++ ys  = x : (xs +++ ys)


-- 6.4  Multiple recursions
-- function applied more than once for each call...

fibonacci    :: Int -> Int
fibonacci 0   = 0
fibonacci 1   = 1
fibonacci n   = fibonacci (n-1) + fibonacci (n-2)


qsort       :: [Int] -> [Int]
qsort []     = []
qsort (x:xs) =
  qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger  = [b | b <- xs, b > x]

--  q [3,2,4,1,5]
--  q [2,1] ++ [3] ++ q [4,5]
--  q [1] ++ [2] ++ q [] ++ [3] ++ [4] ++ q [5]
--  [1,2,3,4,5]


-- 6.5  Mutual recursion

even'    :: Int -> Bool
even' 0   = True
even' n   = odd' (n-1)

odd'     :: Int -> Bool
odd' 0    = False
odd' n    = even' (n-1)


-- even or odd positions in a list:

evens        :: [a] -> [a]
evens []      = []
evens (x:xs)  = x:odds xs

odds         :: [a] -> [a]
odds  []      = []
odds  (_:xs)  = evens xs


-- 6.6   Advice on recursion

-- Program by Numbers,A Programming Method for Novices
-- Hugh Claser, Pieter Hartel, Paul Garratt

-- Step1: define the type
-- Step2: enumerate the cases
-- Step3: define the simple cases
-- Step4: define the other cases
-- Step5: generalize and simplify




-- Produce a list with n identical elements

replicate'    :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x


(!!!)        :: [a] -> Int -> a
(x:_)  !!! 0  = x
(_:xs) !!! n  = xs !!! (n-1)


elem'    :: Eq a => a -> [a] -> Bool
elem' _ []     = False
elem' e (x:xs)
  | e==x       = True
  | otherwise  = elem' e xs
