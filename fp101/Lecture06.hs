-- | Lecture 6 -- Higher Order Functions
-- | Book Chapter 7

module Lecture06 where
import Data.Char (ord,chr)


-- G.1  Basic concepts

add1    :: Int -> Int -> Int
add1 x y = x + y

add2   :: Int -> (Int -> Int)
add2 = \x -> (\y -> x+y)
-- a function that returns a function

twice'     :: (a -> a) -> a -> a
twice' f x  = f (f x)
-- twice' takes a function as it's first parameter

twice''  :: (a -> a) -> a ->a
twice'' f  = f . f

quad :: Int -> Int
quad  = twice' (\x -> x*2)

-- programming idioms can be coded using HOF's
-- domain specific languages can be defined as collections HOF's
-- algerbrac properties of HOF's can be used to reason about programs


-- 6.2  Processing lists

map1,map2 :: (a -> b) -> [a] -> [b]

-- map using list comprehensions:
map1 f xs = [f x | x <- xs]

-- map using recursion
map2 _ []     = []
map2 f (x:xs) = f x : map f xs

lst1 = map (+1) [1,3,5,7]
-- [2,4,6,8]

showEm  :: Show a => [a] -> [String]
showEm   = map show


filter0, filter1  :: (a -> Bool) -> [a] -> [a]

filter0 p xs  = [x | x <- xs, p x]

filter1 _ []  = []
filter1 p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

-- sum of the squares of the even integers
sumsqreven   :: [Int] -> Int
sumsqreven ns  = sum (map (^2)(filter even ns))


-- 6.3  The foldr function

-- f []     = _v
-- f (x:xs) = x (f) f xs

sum1 []          = 0              -- v = 0
sum1 (x:xs)      = x + sum1 xs    -- (f) = +

product1 []     = 1               -- v = 0
product1 (x:xs) = x * product1 xs -- (f) = *

or1  []     = False          -- v = False
or1 (x:xs)  = x || or1 xs    -- (f) = ||

and1 []     = True            -- v = True
and1 (x:xs) = x && and1 xs    -- (f) = &&

-- with foldr
sum2     = foldr (+) 0
product2 = foldr (*) 1
or2      = foldr (||) False
and2     = foldr (&&) True

foldr2       :: (a -> b -> b) -> b -> [a] -> b
foldr2 f v []     = v
foldr2 f v (x:xs) = f x (foldr2 f v xs)
-- replace every occurence of (:) with f
-- and every occurence of [] with v

-- sum [1,2,3]
-- foldr (+) 0 [1,2,3]
-- foldr (+) 0 (1:(2:(3:[])))
-- (1+(2+(3+0)))  => 6

-- homomorphism  -- structure of the list doesn't change

product' :: Num a => [a] -> a
product' = foldr (*) 1

-- product [1,2,3]
-- product (*) 1 [1,2,3]
-- product (*) 1 (1:(2"(3:[])))
-- 1*(2*(3*1))  => 6

-- consume and return a list (silly)
listId :: [a] -> [a]
listId = foldr (:) []


length1,length2,length4   :: [a] -> Int
length1 []     = 0
length1 (_:xs) = 1 + length1 xs

length2 = foldr (\_ n -> 1+n) 0

-- length [1,2,3]
-- length (1:(2:(2:[])))
-- 1+(1+(1+0)) => 3

length4 = sum . map (\_ -> 1)


reverse1,reverse2,reverse3,reverse4
  :: [a] -> [a]

reverse1 []     = []
reverse1 (x:xs) = reverse1 xs ++ [x]

-- (++ ys) = foldr (:) ys

reverse2 []     = []
reverse2 (x:xs) =  snoc x (reverse2 xs)

snoc :: a ->  [a] -> [a]
snoc x []  = [x]
snoc x xs = xs ++ [x]

reverse3 = foldr (\x xs -> xs ++ [x]) []

reverse4 = foldr snoc []

-- replace each {:} by (:) and [] by 'ys'

(+++) :: [a] -> [a] -> [a]
(+++) = foldr (:)

-- reverse [1,2,3]
-- reverse (1:(2:(3:[])))
-- (([] ++ [3]) ++ [2]) ++ [1]
-- [3,2,1]

-- Why is foldr useful?
--  some recursive functions on lists, such a sum are simpler
--  properties of functions defined using foldr can be proved
--   using algebraic properties of foldr such as fusion,
--   banana split rule
-- advanced compiler optimizations can be used.


-- 6.4  The foldl function
sum3 = sum' 0
       where
         sum' acc []     = acc
         sum' acc (x:xs) = sum'(x+acc) xs

-- sum [1,2,3]
-- sum' 0 [1,2,3]
-- sum' (1+0) [2,3]
-- sum' (2+(1+0))[3]
-- sum' (3+(2+(1+0)))[]
-- (3+(2+(1+0))) => 6

-- pattern of recursion:
-- f v []     = v
-- f v (x:xs) = f (v (+) x) xs

sum4     = foldl (+) 0
product3 = foldl (*) 1
or3      = foldl (||) False
and3     = foldl (&&) True

length3  = foldl (\_ n -> n+1) 0
reverse5 = foldl (\xs x -> x:xs) []
-- xs (++!)    = foldl (\ys y -> ys ++ [y]) xs

-- examples:
-- length3 [1,2,3]  => ((0+1)+1)+1
-- reverse5 [1,2,3] => 3:(2:(1:[]))

foldl2    :: (a -> b -> a) -> a -> [b] -> a
foldl2 _ v []     = v
foldl2 f v (x:xs) = foldl2 f (f v x) xs


-- 6.5  The composition operator (.)
(|>)  :: (b -> c) -> (a -> b) -> (a -> c)
f |> g = \x -> f (g x)

odd1,odd2 :: Int -> Bool
odd1 = not |> even

odd2 x = not (even x)

twice2 f = f . f

sumsqrevne1 :: [Integer] -> Integer
sumsqrevne1 = sum . map (^2) . filter even

-- compose a list of functions
compose :: [a-> a] -> (a -> a)
compose  = foldr (.) id


-- other functions
all'   :: (a -> Bool) -> [a] -> Bool
all' prd xs = and [prd x | x <- xs]

any'   :: (a -> Bool) -> [a] -> Bool
any' prd xs = or [prd x | x <- xs]

takeWhile'   :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []   = []
takeWhile' p (x:xs)
  | p x           = x : takeWhile' p xs
  | otherwise     = []

dropWhile'   :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []  = []
dropWhile' p (x:xs)
  | p x          = dropWhile' p xs
  | otherwise    = x:xs


-- 6.6  The string transmitter.

-- Transmit and receive a strings converted into binary

type Bit = Int

-- convert binary to an int
bin2int    :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weights bits]
  where weights = iterate (*2) 1

bin2int'  :: [Bit] -> Int
bin2int'  = foldr (\x y -> x +2 * y) 0

-- convert int to binary
int2bin   :: Int -> [Bit]
int2bin 0  = []
int2bin n  = n `mod` 2 : int2bin(n `div` 2)

-- truncate or extend to 8 bits
make8     :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)


-- Transmission

encode :: String -> [Bit]
--encode = concat . map (make8 . int2bin . ord)
encode = concatMap (make8 . int2bin . ord)

-- chop a string of bits into 8 bit binaries
chop8     :: [Bit] -> [[Bit]]
chop8 []   = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode   :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit  :: String -> String
transmit  = decode . channel . encode

channel  :: [Bit] -> [Bit]
channel = id
