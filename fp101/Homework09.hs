-- | Homework -- Declaring types and classes.
{-# LANGUAGE NPlusKPatterns #-}

module Homework09 where
import Data.List
import Data.Char
import Unsafe.Coerce
-- import Hugs.IOExts (usafeCoerce)

data Nat = Zero
         | Succ Nat
         deriving Show

zero,one,two,three :: Nat
zero  = Zero
one   = Succ Zero
two   = Succ one
three = Succ two

nat2int    :: Nat -> Int
nat2int Zero  = 0
nat2int (Succ n) = 1 + nat2int n

int2nat   :: Int -> Nat
int2nat 0  = Zero
int2nat n  = Succ (int2nat (n-1))


-- Ex (0)  - natToInteger

natToInteger1,natToInteger2,natToInteger3,natToInteger4,
 natToInteger5,natToInteger6,natToInteger7
  :: Nat -> Integer

-- good
natToInteger1 Zero = 0
natToInteger1 (Succ n) = natToInteger1 n + 1

-- good
natToInteger2 (Succ n) = natToInteger2 n + 1
natToInteger2 Zero = 0

-- bad, - bottom type
natToInteger3 n = natToInteger3 n

-- good?
natToInteger4 (Succ n) = 1 + natToInteger4 n
natToInteger4 Zero = 0

-- bad  - fails output, - constant 1
natToInteger5 Zero = 1
natToInteger5 (Succ n ) = (1 + natToInteger5 n) -1

-- good
natToInteger6 = head . m
  where m Zero = [0]
        m (Succ n) = [sum [x | x <- (1 : m n)]]

-- good - counts 'S' ha ha ha
natToInteger7 = \ n -> genericLength [c | c <- show n,c == 'S']

-- bad -  doesn't compile, but good if the signature is changed
natToInteger8 :: Nat -> Int
natToInteger8 = \ n -> length [c | c <- show n, c == 'S']



-- Ex 1  - integerToNat

integerToNat1,integerToNat2,integerToNat5,integerToNat6
  :: Integer -> Nat

-- good
integerToNat1 0 = Zero
integerToNat1 (n+1) = Succ (integerToNat1 n)

-- bad  - bottom type :-)
integerToNat2 0 = Succ Zero
integerToNat2 n = (Succ (integerToNat2 n))

-- bad, doesn't type check
integerToNat3 :: Show a => a -> Integer
integerToNat3 n
  = product [(unsafeCoerce c) :: Integer | c <- show n]

-- bad, do not run
-- integerToNat4 n  = integerToNat4 n

-- good
integerToNat5 (n+1) = Succ (integerToNat5 n)
integerToNat5 0 = Zero

-- good
integerToNat6 (n+1) = let m = integerToNat6 n in Succ m
integerToNat6 0 = Zero

-- bad  - won't compile
integerToNat7 :: Integer -> Integer
integerToNat7 = head . m
  where {
        ; m 0 = [0]
        ; m (n + 1) = [sum [x | x <- (1 : m n)]]
        }

-- bad  - won't compile
--integerToNat8 :: Integer -> Nat
--integerToNat8 = \ n -> genericLength [c | c <- show n, isDigit c]


-- Ex (2)  -- add

add1,add2,add3,add4,add5,add6,add7,add8
  :: Nat -> Nat -> Nat

-- good
add1 Zero n = n
add1 (Succ m) n = Succ (add1 n m)

-- good
add2 (Succ m) n = Succ (add2 n m)
add2 Zero n = n

-- bad  - wrong result
add3 Zero n = Zero
add3 (Succ m) n = Succ (add3 m n)

-- bad  - wrong result
add4 (Succ m) n = Succ (add4 m n)
add4 Zero n = Zero

-- bad  - wrong result
add5 n Zero = Zero
add5 n (Succ m) = Succ (add5 n m)

-- bad  - wrong result
add6 n (Succ m) = Succ (add6 n m)
add6 n Zero = Zero

-- good
add7 n Zero = n
add7 n (Succ m) = Succ (add7 m n)

-- good
add8 n (Succ m) = Succ (add8 m n)
add8 n Zero = n

testAdd fn m n =
  nat2int (fn m n) == nat2int m + nat2int n


-- Ex (3)  - mult

mult1,mult2,mult3
  :: Nat -> Nat -> Nat

-- bad  - wrong result
mult1 Zero Zero = Zero
mult1 m (Succ n) = add1 m (mult1 m n)

-- good
mult2 m Zero = Zero
mult2 m (Succ n) = add1 m (mult2 m n)

-- bad
mult3 m Zero = Zero
mult3 m (Succ n) = add1 n (mult3 m n)

testMult fn m n =
  nat2int (fn m n) == nat2int m * nat2int n


-- Ex (4)

data Tree = Leaf Integer
          | Node Tree Integer Tree

tree4 = Node (Node (Leaf 1) 3 (Leaf 4))
             5
             (Node (Leaf 6) 7 (Leaf 9))

occurs1,occurs2,occurs4,occurs5,occurs6
  :: Integer -> Tree -> Bool

-- good
occurs1 m (Leaf n) = m == n
occurs1 m (Node l n r)
  = case compare m n of
       LT -> occurs1 m l
       EQ -> True
       GT -> occurs1 m r

-- bad  - wrong result
occurs2 m (Leaf n) = m == n
occurs2 m (Node l n r)
  = case compare m n of
       LT -> occurs2 m r
       EQ -> True
       GT -> occurs2 m l

-- bad  - won't compile
-- occurs3 m (Leaf n) = compare m n
-- occurs3 m (Node l n r)
--   = case compare m n of
--        LT -> occurs3 m l
--        EQ -> True
--        GT -> occurs3 m r

-- bad
occurs4 m (Leaf n) = m == n
occurs4 m (Node l n r)
  = case compare m n of
       LT -> occurs4 m l
       EQ -> False
       GT -> occurs4 m r

-- good
occurs5 m (Leaf n) = m == n
occurs5 m (Node l n r)
  | m == n = True
  | m < n =  occurs5 m l
  | otherwise = occurs5 m r

-- bad  - wrong results
occurs6 m (Leaf n) = m == n
occurs6 m (Node l n r)
  | m == n = True
  | m > n =  occurs6 m l
  | otherwise = occurs6 m r

-- bad  - won't compile
-- occurs7 m n = m == n
-- occurs7 m (Node l n r)
--   | m == n = True
--   | m < n =  occurs7 m l
--   | otherwise = occurs7 m r

-- bad - won't compile
-- occurs8 m n = m == n
-- occurs8 m (Node l n r)
--   | m == n = False
--   | m < n =  occurs8 m r
--   | otherwise = occurs8 m l


-- Ex (5)  balanced tree

data Tree5 = Leaf5 Integer
           | Node5 Tree5 Tree5
             deriving (Show)

bin5 = Node5 (Node5 (Leaf5 1) (Leaf5 4))
             (Node5 (Leaf5 6) (Leaf5 9))

bin6 = Node5 (Node5 (Leaf5 1) (Leaf5 4))
             (Node5 (Leaf5 9) (Leaf5 6))


balanced1, balanced4
  :: Tree5 -> Bool
leaves1,leaves4
  :: Tree5 -> Integer

-- bad
leaves1 (Leaf5 x) = x
leaves1 (Node5 l r) = leaves1 l + leaves1 r
balanced1 (Leaf5 _) = True
balanced1 (Node5 l r)
  = abs (leaves1 l - leaves1 r) <= 1 || balanced1 l || balanced1 r

-- won't compile
-- leaves2 (Leaf5 _) = True
-- leaves2 (Node5 l r) = leaves2 l + leaves2 r
-- balanced2 (Leaf5 _) = True
-- balanced2 (Node5 l r) = abs (leaves2 l - leaves2 r) <= 1

-- won't compile
-- leaves3 (Leaf5 _) = True
-- leaves3 (Node5 l r) = leaves3 l + leaves3 r
-- balanced3 (Leaf5 _) = True
-- balanced3 (Node5 l r) = abs (leaves3 l - leaves3 r) <= 1

-- good
leaves4 (Leaf5 _) = 1
leaves4 (Node5 l r) = leaves4 l + leaves4 r
balanced4 (Leaf5 _) = True
balanced4 (Node5 l r)
  = abs (leaves4 l - leaves4 r) <= 1 && balanced4 l && balanced4 r


-- Ex (6) - balance
halve1 :: [Integer] -> ([Integer],[Integer])
balance1 :: [Integer] -> Tree5

-- good
halve1 xs = splitAt (length xs `div` 2) xs
balance1 [x] = Leaf5 x
balance1 xs = Node5 (balance1 ys) (balance1 zs)
  where (ys,zs) = halve1 xs

lst = [1,4,5,6,9]

treea = Node5 (Node5 (Leaf5 1) (Leaf5 4))
              (Node5 (Leaf5 5)
                     (Node5 (Leaf5 6) (Leaf5 9)))

-- bad - won't compile
-- halve2 xs = splitAt (length xs / 2) xs
-- balance2 [x] = Leaf5 x
-- balance2 xs = Node5 (balance2 ys) (balance2 zs)
--   where (ys,zs) = halve2 xs

-- bad - won't compile
-- halve3 xs = splitAt (length xs `div` 2) xs
-- balance3 [x] = Leaf5 x
-- balance3 xs = Node5 ys zs
--   where (ys,zs) = balance3 (halve3 xs)

-- bad - won't compile
-- halve4 xs = splitAt (length xs `div` 2) xs
-- balance4 x = Leaf5 x
-- balance4 xs = Node5 (balance4 ys) (balance4 zs)
--   where (ys,zs) = halve4 xs


-- Ex (7)

data Expr' =  Add Expr' Expr' | Val Int

exp7 = Add (Val 1) (Val 2)


-- Ex (8)

data Tree8 = Leaf8 Int | Node8 Tree8 Tree8

exp8 = Node8 (Leaf8 1) (Leaf8 2)


-- Ex (9)  Maybe Monad

class Monad1 m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
--    (>>) :: m a -> m b -> m b
--    x >> y = x >>= \_ -> y
--    fail :: String -> m a
--    fail msg = error msg

instance Monad1 Maybe where
  return x = Just x
  Nothing >>= _ = Nothing
  (Just x) >>= f = f x


-- Ex (10)  list monad

instance Monad1 [] where
  return x = [x]
  xs >>= f = concat (map f xs)


-- Ex (11)  Monoid

class Monoid1 a where
  mempty :: a
  (<>) :: a -> a -> a

instance Monoid1 [a] where
  mempty = []
  (<>) = (++)


-- Ex (12)  Functor

class Functor1 f where
  fmap :: (a -> b) -> f a -> f b

instance Functor1 Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just a) = Just (f a)


-- Ex (13) Foldable

class (Functor f) => Foldable f where
  fold :: (Monoid1 m) => f m -> m

instance Foldable [] where
  fold  = foldr (<>) mempty
