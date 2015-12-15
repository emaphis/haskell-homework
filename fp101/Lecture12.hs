-- | Lecture12 - Reasoning about programs
-- | Chapter13
{-# LANGUAGE NPlusKPatterns #-}

module Lecture12 where



-- 13.1  Equational reasoning

-- Elementary School
--     x + y == y +x
-- x + (y+z) == (x+y) + z
-- x * (y+z) == x*y + x*z
-- (x+y) * z == x*z + y*z

-- (x+a) * (x+b)  ==  x*x + x(a+b) + a*b


-- 13.2  Reasoning about Haskell

double   :: Int -> Int
double x  = x + x

isZero   :: Int -> Bool
isZero 0  = True
isZero n  = False


-- 13.3  Simple examples ------------------------------

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs)  ++ [x]

-- reverse has no effect of singleton lists:

-- reverse [s]
--  { list notation }
-- reverse (x:[])
--  { applying reverse }
-- (reverse[]) ++ [x]
--  ( applying reverse )
-- [] ++ [x]
--  { applying ++ }
-- [x]

----------------------------------------------------
not'   :: Bool -> Bool
not' False = True
not' True  = False

-- not is it's own inverse:

-- not (not False) = False

-- not (not False)
--  { applying the inner not }
-- not True
--  { applying not}
-- False


-- 13.4  Induction on numbers

data Nat = Zero | Succ Nat

-- inductively defined:
zero = Zero
one  = Succ Zero
two  = Succ (Succ Zero)

-- inductive proofs, property p is true for base case
-- and p is preserved for inductive cases.
-- that the proposition holds for all natural is called
-- the inductive hypothesis


-- forall (n::Nat) P(n)
-- P(Zero)             -- uhmm.
-- P(n) => p(Succ n)   -- you know
-------------------
-- V (n::Nat) P(n)

--------------------------------------------------
add     :: Nat -> Nat -> Nat
add Zero m     = m
add (Succ n) m = Succ (add n m)

-- Prove that V (n::Nat)
-- add n Zero = n

-- P(n) = Add n Zero = n

-- Base case
-- add Zero Zero  -- {uuhm}
--  { applying add }
-- Zero

-- Inductive Steps
-- Induction Hypothesis:  add n Zero = n
-- Goal: add (Succ n) Zero = Succ n

-- add (Succ n) Zero
--  { applying add }
-- Succ (add n Zero)  -- {you know}
--  ( Induction Hypothesis )
-- Succ n


---------------------------------------------------
-- Prove: add x (add y z) = add (add x y) z

-- Base case:
-- add Zero (add y z)
--  { applying outer add }
-- add y z
--  { unapplying add - first clause}
-- = add (add Zero y) z

-- Inductive case:
-- add (Succ x) (add y z)
--  { applying the outer add }
-- Succ add  x (add y z)
-- = add (add (Succ x) y) z
--  { induction hypothesis }
-- Succ (add (add x y) z)
--  { unapplying the outer add }
-- add (Succ (add x y) z)
--  { unapplying the inner add}
-- add (add (Succ x ) y) z



-- For natural numbers
-- P(0)            -- uhmm
-- P(n) => P(n+1)  -- you know
-- V (m>=0) P(m)

-- Example --------------------------------------------
replicate' ::  Int -> a -> [a]
replicate' 0 _  = []
replicate' (n+1) x = x : replicate' n x

-- Prove that   length(replicate n x) = n
-- by induction on n

-- Base case:
-- length (replicate 0 x)
--  {applying replicate}
-- length []
--  {applying length}
-- 0

-- Induction step
-- length (replicate (n+1)x)
--  { applying replicate }
-- length (x:replicate n x)
--   { applying length }
-- 1 + length (replicate n x)
--  { induction hypothesis}
-- 1 + n         -- you know
--  { commutation }
-- n + 1



-- 13.5  Induction on Lists

--  P([])             -- uhmm  { base case }
--  P(xs) => P(x:xs)  -- you know  (assume p(xs) induction hypothesis,
--                                  p(x:xs) inductive step)
--  V (ys) P(ys)


--------------------------------------------------------
-- propery of foldr: (V xs) foldr (:) [] xs = xs

-- base case: foldr (:) [] [] = []
--  { definition of foldr }
-- = []

-- inductive case:
-- assume: foldr (:) [] xs => xs
-- prove foldr (:) [] (x:xs) => (x:xs)
--  { definition of foldr }
-- x : foldr (:) [] xs
--  { inductive hypotheses }
-- = (x:xs)

-------------------------------------------------------
-- Prove: V(xs,ys) length(xs++ys) = length xs + length ys

-- base case: prove
-- length ([] ++ ys)
-- = { applying ++}
-- length ys
-- = { simple arithmetic }
-- 0 + length ys
-- = { unapplying length }
-- length [] + length ys

-- inductive step:
-- prove: length ((x:xs) ++ ys)
-- = { applying ++ }
-- length (x:(xs ++ ys))
-- = { apply length }
-- 1 + length (xs + ys)
-- = {inductive hypothesis }
-- (1+ length xs) + length ys
-- = { unapply length}
-- length (x:xs) + length y

--------------------------------------------------
-- Prove  reverse(reverse xs)  == xs

-- Given reverse(reverse xs) == xs
-- Show that: reverse(reverse(x:xs)) == x:xs

-- base case:
-- reverse (reverse []) => []
--  { applying inner reverse }
-- reverse []
--  { applying reverse }
-- = []

-- inductive case:
-- reverse (reverse (x:xs)) => x:xs
--  { applying inner reverse }
-- reverse (reverse xs ++ [x])
--  { distributivity  -- proved below }
-- reverse [x]  ++ reverse (reverse xs)
--  { singleton lists }
-- [x] ++ reverse (reverse xs)
--  { induction hypothesis }
-- [x] ++ xs
--  { applying ++}
-- x:xs


-- we need contra-variance
-- reverse (xs ++ ys)  == (reverse ys) ++ (reverse xs)

-- base case:
-- reverse ([] ++ ys)
--  { applying ys }
-- reverse ys
--  { unapplying reverse }
-- reverse [] ++ reverse ys

-- inductive case:
-- reverse ((x:xs) ++ ys)
--  { applying ++}
-- reverse (x:(xs++ys))
--  ( induction hypothesis )
-- reverse (xs ++ ys) ++ [x]
--  { associativity of ++}
-- reverse ys ++ (reverse xs ++ [x])
--  { unapplying the second reverse }
-- reverse ys ++ reverse (x:xs)


-- First we need to prove associativity

-- xs ++ (ys ++ zs)  == (xs ++ ys) ++ zs



-- 13.6  Making append vanish

-- slow
reverse1 []   = []
reverse1 (x:xs)  = reverse1 xs ++ [x]

-- create a helper function with induction:

-- Prove : reverseHelper xs ys  == reverse xs ++ ys

-- Base case:
-- reverseHelper [] ys == reverse [] ++ ys
-- == [] ++ ys
-- == ys

-- inductive step
-- given:  reverseHelper xs ys == reverse xs ++ ys
-- show that: reverseHelper (x:xs) ys
-- == reverse (x:xs) ++ ys
-- == reverse xs ++ [x] ++ ys
-- == reverse xs ++ (x:ys)

-- reverseHelper (x:xs) y
-- == reverse (x:xs) ++ ys -- you know
-- == reverseHelper xs (x:ys)

reverseHelper [] ys     = ys
reverseHelper (x:xs) ys = reverseHelper xs (x:ys)

fastReverse xs = reverseHelper xs []


-- fastreverse as a foldr

--fastReverse' xs = foldl fun [] xs

--fun :: [a] -> a -> [a]
--fun xs x = x:xs

--f,f' :: [a] -> [a]
--f []     = _nil
--f (x:xs) =  _cons x (f xs)

--f' = foldr (:) []

-- base case
-- f [] == nil -- assumption
-- == foldr cons nil []  -- foldr

-- inductive step
-- f(x:xs) == cons x (f xs)    -- assumption
-- cons x (foldr cons nil xs)  -- you know
-- foldr cons nil (x:xs)       -- foldr

-- now write reverse as a foldr

-- Nil case
-- reverse []
-- == []  -- definition of reverse
-- == nil -- define nil == []

-- Cons case
-- reverse (x:xs)
-- reverse xs ++ [x]  -- definition of reverse
-- cons x (reverse xs) -- cons = \x -> \xs -> xs++[x]

fastReverse' = foldr (\x -> \xs -> xs ++ [x]) []


id' = foldr (:) []

-- f2 (foldr(:) [] xs) = foldr cons nil xs


-- Alternative approach
-- since we are using pure recursive functions


foldr' cons nil = f
  where f = \xs -> case xs of
                  { []     -> nil
                  ; (y:ys) -> cons y (f ys)
                  }

foldr'' cons nil [] = []
foldr'' cons nil (x:xs) =
  cons x (foldr'' nil cons xs)


foldr3 cons nil = \ xs ->
  case xs of
    []     ->  nil
    (y:ys) -> cons y (foldr3 cons nil ys)

-- foldr4 cons nil = f
--   where f = \ xs ->
--     case xs of


-- overlapping
last' :: [a] -> a
last' [x]  = x
last' (_:xs) = last' xs

-- good
foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 _ v [] = v
foldr2 f v (x:xs) = f x (foldr2 f v xs)

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
--       []     ->  nil
--       (y:ys) -> cons y (f ys)


foldr5 cons nil = fix (\f ->
      \xs -> case xs of
              [] -> nil
              y:ys -> cons y (f ys))


fix :: (a -> a) -> a
fix f = f (fix f)

-- fixed point fusion
-- f(g x) = h (f x)
-- f bot = bot
-- f (fix g) = fix h

-- 13.7  Compiler correctness



-- Papers and books

-- Program Calculation
-- Properties of Continuous Algebra  1991
-- Maarten Fokkinga, Erik Meijer
