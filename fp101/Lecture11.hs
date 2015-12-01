-- | Lecture 11  -- Lazy evaluation
-- | Chapter 12

module Lecture11 where


-- 12.1  Introduction

-- Avoids doing unnecessary evaluation
-- Allows programs to be more modular
-- Allow us to program with infinite data structures

--  Evaluating expressions

square n = n * n

-- square (3 + 4)
-- square 7
-- 7 * 7
-- 49

-- square (3 + 4)
-- (3 + 4) * (3 + 4)
-- 7 * (3 + 4)
-- 7 * 7
-- 49

-- FACT: it doesn't matter which order you use, both return the
-- same result, assuming termination.


-- 12.2  Reduction strategies

-- Redex - reducible subexpression: function applied to arguments

-- Innermost reduction: innermost redex is always reduced
-- Outermost reduction: outermost redex is always reduced

loop :: [a]
loop = tail loop

--fst (l, loop)

-- innermost reduction
-- fst (1, loop)
-- fst (1, tail loop)
-- fst (1, tail (tail loop))
-- fst (1, tail (tail (tail loop)))

-- outermost reduction
-- frst (1, loop)
-- 1

-- If there exists any evaluation sequence that terminates
-- for a given expression, then call-by-name evaluation will
-- also terminate for this expression, and produce the same
-- final result.

-- 12.4  Number of reductions

-- Innermost
-- square (3+4)
-- square 7
-- 7 * 7
-- 49

-- Outermost
-- square (3+4)
-- (3+4) * (3+4)
-- 7 * (3+4)
-- 7 * 7
-- 49

-- Outermost reduction may require more steps than innermost

-- lazy evaluation is Outermost reduction + sharing

-- Never requires more steps than innermost
-- Haskell uses lazy evaluation


-- 12.5  Infinite lists

ones :: [Int]
ones  = 1 : ones

-- ones
-- 1 : ones
-- 1 : 1 : ones
-- 1 : 1 : 1 : ones

-- innermost reduction
-- head ones
-- head (1:ones)
-- head (1:1:ones)
-- head (1:1:1:ones)
-- ...

-- lazy reduction
-- head ones
-- head (1:ones)
-- 1


-- 12.6  Modular programming

-- separates control from data



-- 12.7 Prime number sieve

primes :: [Int]
primes  = sieve [2..]

sieve  :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- 12.6  Modular programming

bool1 = take 10 primes == [2,3,5,7,11,13,17,19,23,29]

-- filter (<15) primes -- produces an infinite list.

bool2 = takeWhile (<15) primes == [2,3,5,7,11,13]


--  Strict application

-- f $! x  - strict form of f x


----------------------------
-- definitions



-- redex = a reducible function application. An expression is
--  in normal form when it contains no further redexes.

-- leftmost redex: that redex whose abstraction or predefined
--  function is textually to the left of all other redexes.
--  There is at most one of these.

-- rightmost redex: that redex whose abstraction or predefined
--  function is textually to the right of all other redexes

-- outermost redex: a redex which is not contained in any
--  other redexes.

-- innermost redex: a redex which contains no other redexes.
--  There can be several of these, hence we talk of leftmost
--  innermost etc.

-- Applicative-order reduction (AOR) : reduce the leftmost
--  innermost redex first

-- Normal-order reduction (NOR) : reduce the leftmost outermost
-- redex first
