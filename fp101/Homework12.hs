-- | Lecture 12 - Reasoning about programs
-- | Chapter 13

module Homework12 where

-- Ex (0) - overlapping patterns
-- patterns are disjoint or non overlapping of order of clauses
-- doesn't matter

-- overlapping
last' :: [a] -> a
last' [x]  = x
last' (_:xs) = last' xs

-- good
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

-- overlapping
init'  :: [a] -> [a]
init' [_]  = []
init' (x:xs) = x : init' xs

-- overlapping
drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_:xs) = drop' (n-1) xs

-- good
(+++) :: [a] -> [a] -> [a]
[] +++ ys = ys
(x:xs) +++ ys = x : (xs +++ ys)

-- good
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ v [] = v
foldl' f v (x:xs) = foldl f (f v x) xs


-- Ex 2 -- add
-- Ex (1) - #2

data Nat = Zero
         | Succ Nat

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)

-- Prove:  add n (Succ m) = Succ (add n m)

-- base case:
-- add Zero (Succ m)
-- = { apply add }
-- Succ m
-- = { unapplying add }
-- Succ (add Zero m)

-- inductive case:
-- add (Succ n) (Succ m)
-- = { applying add }
-- Succ (add  n  (Succ m))
-- = { induction hypothesis }
-- Succ (Succ (add n  m))
-- = { unapplying add}
-- Succ (add (Succ n) m)


-- Ex 3
-- Ex (2)

-- add n Zero => n

-- prove add n m = add m n
-- base case:
-- add Zero m
-- = { applying add }
--  m
-- = { given property of add }
-- = add m Zero

-- inductive case:
-- add (Succ n) m
-- = { apply add }
-- Succ (add n m)
-- = { induction hypothesis }
-- Succ (add m n)
-- = { property of add}
-- add m {Succ n}


-- Ex 4
-- Ex (3)

-- all p []   = True
-- all p (x:xs) = p x && all p xs

--replicate' 0 _  = []
--replicate' (n+1) x = x : replicate' n x

-- prove that replicate produces a a list of identical elements

-- prove: all (== x) (replicate n x) induction on 'n'

-- base case:
-- all (== x) (replicate 0 x)
-- = { apply replicate }
-- all (== x) []
-- = { apply all }
-- True

-- inductive case:
-- all (== x) (replicate (n+1) x)
-- = { applying replicate }
-- all (== x) x : (replicate n x)
-- = { applying all }
-- x == x && all (==x) (replicate n x)
-- = { applying ==}
-- True && all (==x) (replicate n x)
-- = { applying &&}
-- all (== x) (replicate n x)
-- = { inductive hypothesis }
-- True


-- Ex 5
-- Ex (4)

-- [] ++ ys  = ys
-- (x:xs) ++ ys = x:(xs ++ ys)

-- Verify:  xs ++ [] = xs
-- base:
-- [] ++ []
-- = { applying clause one }
-- []

-- inductive
-- (x:xs) ++ [] = x:xs
-- = { apply ++ }
-- x:(xs ++ [])
-- = { induction hypothesis }
-- x : xs


-- Ex (5)

-- verify xs ++ (ys ++ zs) = (xs ++ ys) ++ zs
-- base:
-- [] ++ (ys ++ zs)
-- = { applying ++ }
-- ys ++ zs
-- = { unapplying ++}
-- = ([] ++ ys) ++ zs

-- inductive
-- x:xs ++ (ys ++ zs)
-- = { applying ++ }
-- x:(xs ++ (ys ++ zs))
-- = { induction hypothesis }
-- x:((xs ++ ys) ++ zs)
-- = { unapplying ++}
-- (x:(xs ++ ys)) ++ zs
-- = { unapplying ++}
-- = ((x:xs) ++ ys) ++ zs

-- Ex 6 each step is simpler and creates more theorems


-- Ex 7
-- Ex (6)

-- given:
-- map f []  = []
-- map f (x:xs)  = f x : map f xs
-- and
-- (f . g) x  = f (g x)

-- prove  map f (map g xs)  = map (f . g) xs

-- base case:
-- map f (map g [])
-- = {apply inner map }
-- map f []
-- = { apply map }
-- []
-- = { unapply map }
-- map (f . g) []

-- inductive case:
-- map f (map g x:xs)
-- = { apply map}
-- map f (g x : map g xs)
-- = { apply map}
-- f (g x) : map f (map g xs)
-- = { inductive hypothesis }
-- f (g x) : map (f . g) xs
-- = { unapplying . }
-- (f . g) x : map (f . g) xs
-- = { unapplying map }
-- = map (f . g) x:xs


-- Ex (6)

--length' []     = 0
--length' (_:xs) = 1 + length' xs

--[] ++ ys = ys
--(x:xs) ++ ys = x : (xs ++ ys)

-- prove: length (xs ++ ys) = length xs + length ys

-- base case: prove
-- length ([] ++ ys)
-- = { applying ++}
-- length ys
-- = { simple arithmetic }
-- 0 + length ys
-- = { unapplying length }
-- length [] + length ys

-- inductive case:
-- length ((x:xs) ++ ys)
-- = { applying ++ }
-- length (x:(xs ++ ys))
-- = { apply length }
-- 1 + length (xs ++ ys)
-- = {inductive hypothesis }
-- (1 + length xs) + length ys
-- = { unapply length}
-- length (x:xs) + length y


-- Ex (8)  - induction principle for finite lists:

--  P([])             -- uhmm  { base case }
--  P(xs) => P(x:xs)  -- you know  (assume p(xs) induction hypothesis,
--                         p(x:xs) inductive step)
--  V (ys) P(ys)


-- it is sufficient to show that p holds for the empty list [], the base case,
-- and that if p holds for any list xs,
-- then it also holds for x : xs for *any* element x , the inductive case.


-- Ex (9)

-- length [] = 0
-- length (_:xs) = 1 + length xs

-- take 0     _      = []
-- take (n+1) []     = []
-- take (n+1) (x:xs) = x : take n xs

-- repeat x = x : repeat x

-- Prove length (take n (repeat x)) = n

-- base case:
-- length (take 0 (repeat x))
-- = { applying take }
-- length []
-- = { applying length }
-- 0

-- inductive case:
-- length (take (n+1) (repeat x))
-- = { applying repeat }
-- length (take (n+1) (x : repeat x))
-- = { applying take }
-- length (x : take n (repeat x))
-- = { applying length }
-- 1 + length (take n (repeat x))
-- = { inductive hypothesis }
-- 1 + n
-- = { commutativity of + }
-- n + 1
