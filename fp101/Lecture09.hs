-- | Lecture 9  -- Declaring Types and Classes
-- | Chapter 10

module Lecture09 where


-- 10.1  Types and declarations

-- a new name for an old type
type String1 = [Char]

-- can make other type easier to read
type Pos = (Int,Int)

origin  :: Pos
origin   = (0,0)

left      :: Pos -> Pos
left (x,y) = (x-1,y)

-- type can also have parameters
type Pair a = (a,a)

type Pint = Pair Int  -- world of type level

mult      :: Pair Int -> Int -- world of value level
mult (m,n) = m*n

copy  :: a -> Pair a
copy x = (x,x)

-- types can be nested:
type Trans = Pos -> Pos

left'      :: Trans
left' (x,y) = (x-1,y)

-- type cannot be recursive:
--type Tree = (Int,[Tree])


-- 10.2  Data declarations
-- Algerbriac data types
-- a new type specified by its values.
data Bool' = False' | True'
-- True and False are constructors of type Bool'
-- must begin with upper-case letters
-- similar to context free grammers.


data Answer  = Yes | No | Unknown

answers     :: [Answer]
answers      = [Yes,No,Unknown]

flip'        :: Answer -> Answer
flip' Yes     = No
flip' No      = Yes
flip' Unknown = Unknown


-- can be used like built in types
data Move = Left' | Right' | Up' | Down'

move            :: Move -> Pos -> Pos
move Left' (x,y)  = (x-1,y)
move Right' (x,y) = (x+1,y)
move Up' (x,y)    = (x,y-1)
move Down' (x,y)  = (x,y+1)

moves         :: [Move] -> Pos -> Pos
moves [] p     = p
moves (m:ms) p = moves ms (move m p)

flip      :: Move -> Move
flip Left'   = Right'
flip Right'  = Left'
flip Up'     = Down'
flip Down'   = Up'

-- constructors can also have arguments
data Shape = Circle Float
           | Rect Float Float

square   :: Float -> Shape
square n  = Rect n n

area     :: Shape -> Float
area (Circle r) = pi * r*r
area (Rect x y) = x * y

-- Circle and Rect can be viewed a functions that construct values of Shape
-- Circle :: Float -> Shape
-- Rect   :: Float -> Float -> Shape

-- data definitions can also take parameters
data Maybe' a = Nothing' | Just' a

safediv :: Int -> Int -> Maybe Int
safediv _ 0  = Nothing
safediv m n  = Just (m `div` n)

safehead     :: [a] -> Maybe a
safehead []   = Nothing
safehead xs   = Just (head xs)


-- 10.3  Recursive types

data Nat = Zero | Succ Nat

zero,one,two,three,inf :: Nat
zero = Zero
one = Succ Zero
two = Succ one
three = Succ two

inf = Succ inf  -- whoah

four  = Succ (Succ (Succ (Succ Zero)))
four' = 1 + (1 + (1 + (1 + 0)))

nat2int    :: Nat -> Int
nat2int Zero  = 0
nat2int (Succ n) = 1 + nat2int n

int2nat   :: Int -> Nat
int2nat 0  = Zero
int2nat n  = Succ (int2nat (n-1))

add           :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

-- add (Succ (Succ Zero)) (Succ Zero)
-- Succ (add (Succ Zero) (Succ Zero))
-- Succ (Succ (add Zero (Succ Zero)))
-- Succ (Succ (Succ Zero))


-- a list definition
data List a = Nil | Cons a (List a)

len            :: List a -> Int
len Nil         = 0
len (Cons _ xs) = 1 + len xs

-- Expressions

data Expr = Val Int
          | Add Expr Expr
          | Mul Expr Expr

expr1 = Add (Val 1) (Mul (Val 2) (Val 3))

size          :: Expr -> Int
size (Val n)   = 1
size (Add x y) = size x + size y
size (Mul x y) = size x + size y

eval          :: Expr -> Int
eval (Val n)   = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

--eval' = foldl id (+) (*)


-- Binary Tree
data Tree = Leaf Int
          | Node Tree Int Tree

tree1 = Node (Node (Leaf 1) 3 (Leaf 4))
             5
             (Node (Leaf 6) 7 (Leaf 9))

-- search the tree for a given integer
occurs               :: Int -> Tree -> Bool
occurs m (Leaf n)     = m==n
occurs m (Node l n r) = m==n
                        || occurs m l
                        || occurs m r

-- flatten a tree into a list
flatten              :: Tree -> [Int]
flatten (Leaf n)      = [n]
flatten (Node l n r)  = flatten l
                        ++ [n]
                        ++ flatten r
-- tree is a search tree if 'flatten' returns a sorted list
-- occurs for a search tree
occurs'               :: Int -> Tree -> Bool
occurs' m (Leaf n)            = m==n
occurs' m (Node l n r) | m==n = True
                       | m<n  = occurs' m l
                       | m>n  = occurs' m r

-- 10.4  Tautology checker



-- 10.5  Abstract machine



-- 10.6  Class and instance declarations
