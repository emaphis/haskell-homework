-- | Homework for Lecture 11, Chapter 12 - Lazy evaluation

module Homework11 where


-- Ex 1  redexes

-- Ex (0)
-- 1 + (2*3)
-- (2*3) - both innermost and outermost

-- Ex (1)
-- (1+2) * (2+3)
-- (1+2)  - innermost and outermost
-- (2+3)  -

-- Ex (2)
-- fst (1+2, 2+3)
-- 1+2  - innermost
-- 2+3
-- fst (1+2, 2+3) - outermost

-- Ex (3)
-- (\x -> 1+x) (2*3)
-- (2*3)  - innermost
-- rest outermost


-- Ex 2.
-- Ex (4)
-- outermost:  (fewer steps)
-- fst (1+2, 2+3)
-- 1+2
-- 3

-- innermost: (more steps)
-- fst (1+2, 2+3)
-- fst (3, 2+3)
-- fst (3, 5)
-- 3


-- Ex 3
-- Ex (5)
mult = \x -> (\y -> x * y)

--  mult 3 4
-- (\x -> (\y -> x * y)) 3 4
-- (\y -> 3 * y) 4
-- 3 * 4
-- 12


-- Ex 4  -- infinite fibs using list comprehension
-- Ex (6)
fibs :: [Integer]
fibs = 0 : 1 :[ x+y | (x,y) <- zip fibs (tail fibs)]


-- Ex 5  - nth Fibonacci
-- Ex (7)
fibo :: Int -> Integer
fibo n = fibs !! n

-- Ex (8)  - first fibo after 1000
largeFib,largeFib2,largeFib3,largeFib4  :: Integer
largeFib  = head (dropWhile (<= 1000) fibs)
-- => 1597

largeFib2 = last (take 18 fibs)

largeFib3 = head (filter (> 1000) fibs)

largeFib4 = head (drop 17 fibs)


-- Ex 6

data Tree a = Leaf
            | Node (Tree a) a (Tree a)


-- Ex (9)  repeatTree

repeatTree2
  :: a -> Tree a

-- bad - wont compile
--repeatTree1 x = Node x x x

-- good
repeatTree2 x = Node t x t
  where t = repeatTree2 x

-- bad  - won't compile
--repeatTree3 x = repeatTree3 (Node Leaf x Leaf)

-- bad  - won't compile
--repeatTree4 x = Node t x t
--  where t = repeatTree4 (Node t x t)


-- Ex (10)  -- innermost reduction

--outermost will always terminate, if termination is possible
-- outermost may require more steps than innermost
-- lazy - outermost with sharing - used be haskell
-- outermost+sharing never requires more steps than innermost

-- innermost may require fewer steps then outermost ***


-- Ex (11)  - outermost reduction

-- May terminate when innermost does not


-- Ex (12)  - Lazy evaluation

-- Makes some programs more efficient than otherwise


-- Ex (13) - Haskell

-- function application associates to the left
