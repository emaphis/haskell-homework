-- | Homework for week one

module Homework01 where

-- Example
-- return the sum of the integers (whole numbers) between one
-- and some larger number n

sum1 n =
  if n==0
  then 0
  else n + sum1 (n-1)

sum2 []     = 0
sum2 (x:xs) = x + sum xs


-- Exerciese 1.7

-- Ex (4)
-- 1.1. Give another possible calculation for the result of double (double 2)
-- double x = x + x
-- double (double 2)
-- (double 2) + (double 2) {applying outer double}
-- 2 + 2 +  (double 2)     {applying first double}
-- 4 + (double 2)          {applying +}
-- 4 + 2 + 2               {applying double}
-- 8                       {applying +}

-- Ex (5)
-- 1.2. Show that sum [x] = x for any number x.
-- given:
-- sum []     = 0
-- sum (x:xs) = x + sum xs
--
-- sum [x]
-- x + sum []  {apply second clause}
-- x + 0       {apply first clause}
-- x           {appply +}

-- Ex (6)
-- 1.3. Define a function product that produces the product of a list of numbers
-- and show using your definition that product [2, 3, 4] = 24.

product1 []      = 1
product1 (x:xs)  = x * product1 xs

-- product1 [2,3,4]
-- 2 * product1 [3,4]
-- 2 * 3 * product1 [4]
-- 2 * 3 * 4 * product1 []
-- 2 * 3 * 4 * 1
-- 24

-- Ex (7)
-- 1.4. How should the definition of the function qsort be modified so that it
-- produces a reverse sorted version of a list?

rqsort1 []     = []      -- good - my answer
rqsort1 (x:xs) = rqsort1 lrg ++ [x] ++ rqsort1 sml
  where sml = [a | a <- xs, a <= x]
        lrg = [b | b <- xs, b > x]

rqsort2 []     = []     -- bad -- just mixing it up
rqsort2 (x:xs) = reverse (rqsort2 sml ++ [x] ++ rqsort2 lrg)
  where sml = [a | a <- xs, a <= x]
        lrg = [b | b <- xs, b > x]

rqsort3 []     = []     -- bad, some of the time
rqsort3 (x:xs) = rqsort3 lrg ++ rqsort3 sml ++ [x]
  where sml = [a | a <- xs, a <= x]
        lrg = [b | b <- xs, b > x]

rqsort4 []     = []     -- bad
rqsort4 (x:xs)
  = reverse (rqsort4 sml) ++ [x] ++  reverse (rqsort4 lrg)
  where sml = [a | a <- xs, a <= x]
        lrg = [b | b <- xs, b > x]

rqsort5 []     = []     -- good!!
rqsort5 (x:xs) = rqsort5 lrg ++ [x] ++ rqsort5 sml
  where lrg = [a | a <- xs, a > x || a ==x]
        sml = [b | b <- xs, b < x]

rqsort6 []     = []     -- bad - skips equals
rqsort6 (x:xs) = rqsort6 lrg ++ [x] ++ rqsort6 sml
  where sml = [a | a <- xs, a < x]
        lrg = [b | b <- xs, b > x]

rqsort7 []     = []     -- good
rqsort7 (x:xs)
  = reverse
    (reverse (rqsort7 sml) ++ [x] ++ reverse(rqsort7 lrg))
  where sml = [a | a <- xs, a <= x]
        lrg = [b | b <- xs, b > x]

rqsort8 []     = []     -- bad
rqsort8 xs = x : rqsort8 lrg ++  rqsort8 sml
  where x = maximum xs
        sml = [a | a <- xs, a < x]
        lrg = [b | b <- xs, b >= x]

rqsort1' []     = []
rqsort1' (x:xs) = rqsort1' lrg ++ [x] ++ rqsort1' sml
  where sml = [a | a <- xs, a <= x]
        lrg = [b | b <- xs, b > x]

test_revqsort fn =
  fn []        == []      &&
  fn [1]       == [1]     &&
  fn [1,1]     == [1,1]   &&
  fn [1,2]     == [2,1]   &&
  fn [1,2,3]   == [3,2,1] &&
  fn [1,5,2,8,1] == [8,5,2,1,1]


-- Ex (8)
-- 1.5. What would be the effect of replacing <= by < in the
--    definition of qsort?
-- Hint: consider the example qsort [2, 2, 3, 1, 1].
qsortbad []     = []
qsortbad (x:xs) = qsortbad ys ++ [x] ++ qsortbad zs
  where ys = [a | a <- xs, a < x]
        zs = [b | b <- xs, b > x]
-- it skips equals


-- Chapter 2 first steps

-- Exerciese  2.6

-- Ex (0)
-- 2.1. Parenthesise the following arithmetic expressions:
-- (2 ^ 3) ∗ 4
-- (2 ∗ 3) + (4 ∗ 5)
-- 2 + (3 ∗ (4 ^ 5))

-- 2.2. Work through the examples from this chapter using Hugs.

-- Ex (1)
-- 2.3. The script below contains three syntactic errors. Correct these errors and
-- then check that your script works properly using Hugs.
n :: Int
n = a `div` length xs
  where a = 10
        xs = [1, 2, 3, 4, 5]

m :: Int
m = a `div` length xs
 where
    a = 10
    xs = [1, 2, 3, 4, 5]


-- a and xs don't align, backquotes on div, 'm' should be lower case

-- Ex (2)
-- 2.4. Show how the library function last that selects the last element of a nonempty
-- list could be defined in terms of the library functions introduced in
-- this chapter. Can you think of another possible definition?

-- list functions:
-- head xs, tail xs, xs !! n, take n xs, drop n xs,
-- lenght xs, sum xs, product xs, xs ++ ys, reverse xs

last1 xs = drop (length xs - 1) xs        -- bad

last2 xs = head (drop (length xs - 1) xs) -- good

last3 xs = tail (reverse xs)              -- bad

last4 xs = reverse (head xs)              -- bad wrong type

last5 xs = xs !! (length xs - 1)          -- good

last6 xs = head (drop (length xs) xs)     -- bad, bad, bad

last7 xs = head (reverse xs)              -- good - my design

last8 xs = reverse xs !! (length xs - 1)  -- bad - a usable first though

last2' :: [a] -> a  -- my solution
last2' xs = head (drop (length xs - 1) xs)

last7' :: [a] -> a -- my solution
last7' xs = head (reverse xs)

test_last fn =
  fn [1]     == 1 &&
  fn [1,2]   == 2 &&
  fn [1,2,3] == 3

-- Ex (3).
-- 2.5 Show how the library function init that removes the last element from a
-- non-empty list could similarly be defined in two different ways.

init1 xs = tail (reverse xs)              -- bad, reverses list

init2 xs = reverse (head (reverse xs))    -- bad, wrong type

init3 xs = reverse (tail xs)              -- bad, reverse tail

init4 xs = take (length xs) xs            -- bad, list identity function

init5 xs = reverse (tail (reverse xs))    -- good, my design

init6 xs = take (length xs - 1) (tail xs) -- bad

init7 xs = drop (length xs - 1) xs        -- bad

init5' :: [a] -> [a]
init5' xs = reverse (tail (reverse xs))

init2' :: [a] -> [a]
init2' xs = take (length xs - 1) xs

test_init fn =
  fn [1]       == [] &&
  fn [1,2]     == [1] &&
  fn [1,2,3]   == [1,2] &&
  fn [1,2,3,4] == [1,2,3]

