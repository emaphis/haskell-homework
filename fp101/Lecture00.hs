-- | Lecture 0 - Introduction

module Lecture00 where

add3 :: Num a => a -> a
add3 num = 3 + num

sum1 :: Integer
sum1 = sum [1..100]

lst :: [Integer]
lst = [1,2,3,4]


-- Historical background:
-- 1930s  Alonzo Church develops lambda calculus
-- 1950s  John McCarthy develops Lisp
-- 1960s  Peter Landin develops ISWIM
-- 1970s  John Backus FP higher order functions
--        Robin Milner ML type inference, polymorphic type
-- 1980s  David Turner Miranda - Lazy,pure functional programming
--        Commitee  Haskell


f :: Ord t => [t] -> [t]
f []     = []
f (x:xs) = f ys ++ [x] ++ f zs
  where ys = [a | a <- xs, a<=x]
        zs = [b | b <- xs, b > x]

