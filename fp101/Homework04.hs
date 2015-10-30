-- | Homework for Lecture 4 and Chapter 5
-- | List comprehensions

module Homework04 where
import Data.Char

-- Ex (0)
-- Ex 5.1 - calculate the sum of the squares from 1 to 100

sum100 = sum [n^2 | n <- [1..100]]
-- 338350

-- no compile
-- sum100a = sum [[x*x] | x <- [1..100]]

-- good
sum100b = sum [x^2 | x <- [1..100]]

-- wrong answer... 200  2*100
sum100c = sum [const 2 x | x <- [1..100]]

-- wrong answer - wrong base case. - off by one
sum100d = foldl (+) (1) [x^2 | x <- [1..100]]

test_sum100 x  = x == 338350


-- Ex (1)
-- Ex 5.2 - relicate

replicate', replicate4  :: Int -> a -> [a]
replicate' n x = [x | _ <- [1 .. n]]

-- bad output
replicate1 :: (Num t1, Enum t1) => Int -> t -> [Bool]
replicate1 n a = [True | _ <- [1 .. n]]

-- bad output
replicate2 :: (Num t1, Enum t1) => t1 -> t -> [t1]
replicate2 n a = [n | _ <- [1..n]]

-- bad output, incorrect type
replicate3 :: (Num t1, Enum t1) => t -> t1 -> [t1]
replicate3 n a = [a | _ <- [1..a]]

-- good
replicate4 n a = [a | _ <- [1..n]]

test_replicate :: (Int -> Bool -> [Bool]) -> Bool
test_replicate fn =
  fn 3 True == [True,True,True] &&
  fn 3 False == [False,False,False] &&
  fn 0 True  == []

-- replicate' 3 True  => [True,True,True]


-- Ex (2)
-- Ex 5.3 - pythoagorean triple:

pyths,pyths1,pyths2,pyths3  :: Int -> [(Int,Int,Int)]
pyths n =
  [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n],
   (x^2 + y^2 == z^2)]

pyths1 n =  -- bad no results
  [(x,y,z) | x <- [1..n], y<-[1..x], z<-[1..y],
   x^2 + y^2 == z^2]

pyths2 n =  -- bad output
  [(x,y,x) | x<-[1..n], y<-[x..n], z<-[y..n],
   x^2 + y^2 == z^2]

pyths3 n =  -- good
  [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n],
   x^2 + y^2 == z^2]

--  pyths4 -- bad

test_pyths fn =
  fn 10 == [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]


-- Ex (3)
-- Ex 5.4  - perfect numbers

factors  :: Int -> [Int]
factors n =
  [x | x <- [1..n], n `mod` x == 0]

perfect :: Int -> Bool
perfect n = n == sum (init (factors n))

perfects,perfects',perfects1,perfects2  :: Int -> [Int]

perfects n = [x | x <- [1..n], perfect x]
perfects' n = [x | x<-[1..n], sum(init (factors x)) == x]

-- bad
perfects1 n = [x | x<- [1..n], isPerfect x]
  where isPerfect num = sum (factors num)  == num

-- good
perfects2 n = [x | x<- [1..n], isPerfect x]
  where isPerfect num = sum (init(factors num))  == num

test_perfects :: (Num t, Num a, Eq t) => (a -> [t]) -> Bool
test_perfects fn = fn 500 == [6,28,496]


-- Ex (4)
-- Ex 5.5  - redefine lst1 with two nested comprehensions

lst1 = [(x,y) | x <- [1,2,3], y <- [4,5,6]]

lst2 = concat [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]

-- missing concat
lsta = [z | z <- [[(x,y) | y <- [4,5,6]] | x<- [1,2,3]]]


-- Ex (5)
-- Ex 5.6  - redefine postions using find.

find    :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k==k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs =
  [i | (x', i) <- zip xs [0..n], x==x']
  where n = length xs - 1

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs =  find x (zip xs [0..n])
  where n = length xs - 1


-- Ex (6)
-- Ex 5.7 - scalar product

scalarproduct, scalarproduct' :: [Int] -> [Int] -> Int
scalarproduct xs ys =
  --sum [x*y | (x,y) <- (zip [x | x<-xs] [y | y<-ys])]
  sum [x*y | (x,y) <- zip xs ys]

scalarproduct' xs ys = sum [x*y | (x,y) <- xs `zip` ys]

--  scalarproduct [1,2,3] [4,5,6]  => 32


-- Ex (7)
-- Ex 5.8  - modify the Caesar cipher program to handle upper case

low2int   :: Char -> Int
low2int c  = ord c - ord 'a'

int2low   :: Int -> Char
int2low n  = chr (ord 'a' + n)

upp2int   :: Char -> Int
upp2int c  = ord c - ord 'A'

int2upp   :: Int -> Char
int2upp n  = chr (ord 'A' + n)

letters   :: String -> Int
letters xs  = length [x | x <- xs, isAlpha x]

count     :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x==x']

percent     :: Int -> Int -> Float
percent n m  = (fromIntegral n / fromIntegral m) * 100


shift     :: Int -> Char -> Char
shift n c
  | isLower c   = int2low ((low2int c + n) `mod` 26)
  | isUpper c   = int2upp ((upp2int c + n) `mod` 26)
  | otherwise   = c

freqs     :: String -> [Float]
freqs xs   = [percent (count x xs') n | x <- ['a'..'Z']]
             where
               xs' = map toLower xs
               n   = letters xs

encode     :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

lst3 = "Think like a Fundamentalist Code like a Hacker"
lst4 = "Guvax yvxr n Shaqnzragnyvfg Pbqr yvxr n Unpxre"


-- Ex (8)

lst8 = [(x, y) | x <- [1, 2], y <- [1, 2]]
-- [(1,1),(1,2),(2,1),(2,2)]

-- Ex (9)

lst9 = [x | x <- [1, 2, 3], y <- [1..x]]
-- [1,2,2,3,3,3]

-- Ex (10)

int10 = sum [x | x <- [1..10], even x]  -- 30
--  [2,4,6,8,10]

-- Ex (11)
xs = 1 : [x + 1 | x <- xs]  -- infinite list
-- take 10 xs


-- Ex (12)  -- riffle

riffle xs ys = concat [[x,y] | (x,y) <- xs `zip` ys]


-- Ex (12)  -- devisors

divides :: Int -> Int -> Bool
divides n m  = n `mod` m  == 0

divisors x = [d | d <- [1..x], x `divides` d]
