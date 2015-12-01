-- | Lecture 10  -- The countdown problem
-- | Chapter 11 

module Lecture10 where


-- 11.1  Introduction

-- Typical transformation development of a program.

-- 11.2  Formalising the Problem

-- The operators:
data Op = Add | Sub | Mul | Div

-- is operation valid, applying an operator two
-- positives results in a positive
valid         :: Op -> Int -> Int -> Bool
valid Add _ _  = True
valid Sub x y  = x > y
valid Mul _ _  = True
valid Div x y  = x `mod`y == 0

-- apply an operator
apply         :: Op -> Int -> Int -> Int
apply Add x y  = x + y
apply Sub x y  = x - y
apply Mul x y  = x * y
apply Div x y  = x `div` y


data Expr  = Val Int | App Op Expr Expr

-- return a list of all the values of an expression
values             :: Expr -> [Int]
values (Val n)      = [n]
values (App _ l r)  = values l ++ values r

-- return the overal value of an expression, provided
-- that it is a positive natural number
-- return [x] for succuess, [] for failure
eval               :: Expr -> [Int]
eval (Val n)        = [n | n > 0]
eval (App o l r)    = [apply o x y | x <- eval l,
                                     y <- eval r,
                                     valid o x y]
                      
-- return all the subseuences of a list
subs               :: [a] -> [[a]]
subs []             = [[]]
subs (x:xs)         = yss ++ map (x:) yss
                       where yss = subs xs

-- returns all possible ways of inserting an
-- elemtent into a list
interleave         :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys):map (y:) (interleave x ys)

-- returns all permutaions of alist
perms              :: [a] -> [[a]]
perms []            = [[]]
perms (x:xs)        = concat (map (interleave x) (perms xs))

-- return a list of all possible ways of choosing zero
-- or more elements from a list
choices            :: [a] -> [[a]]
choices xs          = concat (map perms (subs xs))

-- return a solution to a problem
solution        :: Expr -> [Int] -> Int -> Bool
solution e ns n
  = elem (values e) (choices ns) && eval e == [n]


-- ll.3 Brute forse solution

-- return all possible way of splitting a list
split       :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs)
  = ([x],xs):[(x:ls,rs) | (ls,rs) <- split xs]

-- returns all possible expressions whose list of values
-- is precisely a given list:
exprs      :: [Int] -> [Expr]
exprs []    = []
exprs [n]   = [Val n]
exprs ns    = [e | (ls,rs) <- split ns,
                    l <- exprs ls,
                    r <- exprs rs,
                    e <- combine l r]

combine     :: Expr -> Expr -> [Expr]
combine l r  = [App o l r | o <- ops]

ops         :: [Op]
ops          = [Add,Sub,Mul,Div]


-- return all possible solutions
solutions      :: [Int] -> Int -> [Expr]
solutions ns n  = [e | ns' <- choices ns,
                       e <- exprs ns',
                       eval e == [n]]


-- 11.4  Combind generation and evaluation

-- result of express that evalutas successfully,
-- paired with overall results
type Result = (Expr,Int)

-- returns all possible results comprising expressions
-- whoselist ov values is precisely a given list
results    :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [res | (ls, rs) <- split ns,
                     lx <- results ls,
                     ry <- results rs,
                     res <- combine' lx ry]

-- combine each pair of results using each of the four=
-- numeric operators

combine'           :: Result -> Result -> [Result]
combine' (l,x)(r,y) = [(App o l r, apply o x x) | o <- ops,
                                                  valid o x y]


solutions'     :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns,
                       (e,m) <- results ns',
                       m == n]

-- 11.5

-- strengthing the 'valid' operation to take advantage of semitries
-- of arithmetic

valid'         :: Op -> Int -> Int -> Bool
valid' Add x y  = x <= y
valid' Sub x y  = x > y
valid' Mul x y  = x <= y && x == 1 && y == 1
valid' Div x y  =  x `mod` y == 0
