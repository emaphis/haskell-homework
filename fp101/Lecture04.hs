-- | Lecture04 -- List Comprehensions
-- | Book Chapter 5

module Lecture04 where
import Data.Char

-- List comprehensions are code that manipulate data structures.

-- 4.1   Generatorsg

-- {x2 | x ∈ {1 . . 5}}

-- x <- [1..5] is a generator
gen1 = [x^2 | x <- [1..5]]

-- comprehensions can have multiple generators
gen2 = [(x,y) | x <- [1,2,3], y <- [4,5]]

gen3 = [(x,y) | y <- [4,5], x <- [1,2,3]]

-- gen2 - [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]
-- gen3 - [(1,4),(2,4),(3,4),(1,5),(2,5),(3,5)]

lsta = [[x]++[y] |  x <- "outer loop", y <- "inner loop" ]

-- generators can depend on other generators
gen4 = [(x,y) | x <- [1..3], y <- [x..3]]

-- gen4 - [(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]

-- concat
concat'  :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

lst1 = concat' [[1,2,3], [4,5], [6]]
-- [1,2,3,4,5,6]

-- you can discard parts of a list

firsts   :: [(a,b)] -> [a]
firsts ps = [x | (x,y) <- ps]
-- firsts [(1,2),(3,4),(5,6)] =>[1,3,5]

length'   :: [a] -> Int
length' xs = sum [1 | _ <- xs]
-- length' [1,2,3,4,5,6]   => 6


-- 4.2   Guards
-- guards filter the results of generators

lst2 = [x | x <- [1..10], even x]
-- [2,4,6,8,10]

factors  :: Int -> [Int]
factors n =
  [x | x <- [1..n], n `mod` x == 0]

fcs1 = factors 15
-- [1,3,5,15]
fcs2 = factors 7
-- [1,7]


prime ::  Int -> Bool
prime n = factors n == [1,n]

-- prime 15  => false
-- prine 7   => true

primes  :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

prms1 = primes 40
-- [2,3,5,7,11,13,17,19,23,29,31,37]

-- find a value based on a key
find    :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k==k']

lst4 = find 'b' [('a',1), ('b',2),('c',3),('b',4)]
-- [2,4]


-- 4.3   The zip function
--zip' :: [a] -> [b] -> [(a,b)]
--zip' xs ys =
--  [(x | x<-xs, y | y<-ys)]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

lst3 = pairs [1,2,3,4]
-- [(1,2),(2,3),(3,4)]

sorted  :: Ord a => [a] -> Bool
sorted xs =
  and [x<=y | (x,y) <- pairs xs]

bool1 = sorted [1,2,3,4]  -- True
bool2 = sorted [1,3,2,4]  -- False


-- positions of a element of a list:
positions :: Eq a => a -> [a] -> [Int]
positions x xs =
  [i | (x', i) <- zip xs [0..n], x==x']
  where n = length xs - 1


-- 4.4   String comprehensions
-- strings a lists of characters
chr1 = "abcde" !! 2 -- 'c'
int1 = length "abcde"  -- 5
int2 = take 3 "abcde"  -- "abc"

-- count the number of lower case chars in a string:
lowers  :: String -> Int
lowers xs =
  length [x | x <- xs, isLower x]

count     :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x==x']



-- 4.5   The Caesar cipher

-- Encoding and decoding

let2int   :: Char -> Int
let2int c  = ord c - ord 'a'

int2let   :: Int -> Char
int2let n  = chr (ord 'a' + n)

-- shift letter by a shift factor - rapping around
shift          :: Int -> Char -> Char
shift n c
  | isLower c   = int2let((let2int c + n) `mod` 26)
  | otherwise   = c


encode     :: Int -> String -> String
encode n xs = [shift n x | x<-xs]


-- Frequency tables:

table  :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
        6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent     :: Int -> Int -> Float
percent n m  = (fromIntegral n / fromIntegral m) * 100


freqs   :: String -> [Float]
freqs xs =
  [percent (count x xs) n | x <- ['a' .. 'z']]
  where n = lowers xs

lst5 = freqs "abbcccddddeeeee"
-- [6.67,13.34,20.0,26.68,33.36,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
--0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]


-- Cracking the cipher

-- Chi square
chisqr       :: [Float] -> [Float] -> Float
chisqr os es  = sum [((o-e)^2)/e | (o,e) <- zip os es]


-- rotate a list 'n' places to the left
rotate      :: Int -> [a] -> [a]
rotate n xs  = drop n xs ++ take n xs

lst6 = rotate 3 [1,2,3,4,5]
--              [4,5,1,2,3]

crack    :: String -> String
crack xs  = encode (-factor) xs
  where factor = head (positions (minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs

--  crack "kdvnhoo lv ixq"
--  "haskell is fun"

-- crack "vscd mywzboroxcsyxc kbo ecopev"
-- "list comprehensions are useful"
