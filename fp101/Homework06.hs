-- | Homework for Lecture 6 and Chapter 7
-- | Higher order functions

module Homework06 where
import Data.Char (ord,chr)

-- Ex (0)
-- Ex 7.1  list comprehension

fun1,fun2 :: (a -> b) -> (a -> Bool) -> [a] -> [b]

fun1 f p xs = [f x | x <- xs, p x]
fun2 f p xs = map f (filter p xs)


-- Ex 7.2  - define all any takeWhile dropWhile

all'     :: (a -> Bool) -> [a] -> Bool
all' p xs  =  and (map p xs)
-- and . map p

-- Ex (1)
all1,all3,all4,all6,all7,all8
  :: (a -> Bool) -> [a] -> Bool

-- good
all1 p xs  = and (map p xs)

-- Bad  - won't compile
--all2 p xs = map p (and xs)

-- good
all3 p = and . map p

-- good
all4 p = not . any (not . p)

-- Bad  - won't compile
--all5 p = map p . and

-- good
all6 p xs = foldl (&&) True (map p xs)

-- Bad  -- wrong result
all7 p xs = foldr (&&) False (map p xs)

-- good
all8 p = foldr (&&) True . map p

testAll fn =
  fn even []         == True  &&  -- base case
  fn even [1]        == False &&
  fn even [2]        == True  &&
  fn even [2,4,6,8]  == True  &&
  fn even [2,3,6,8]  == False

any'     :: (a -> Bool) -> [a] -> Bool
any' p xs  = or (map p xs)
-- or . map p

-- Ex (2) -- also experiment with partial ane infinite lists
any2,any3,any4,any5,any6,any7,any8
  :: (a -> Bool) -> [a] -> Bool

-- Bad - doesn't comile
--any1 p = map p . or

-- good
any2 p = or . map p

-- good
any3 p xs = length (filter p xs) > 0

-- good
any4 p = not . null . dropWhile (not . p)

-- bad - wrong result
any5 p = null . filter p

-- good
any6 p xs = not (all (\x -> not (p x)) xs)

-- good
any7 p xs = foldr (\x acc -> (p x) || acc) False xs

-- bad -- wrong results
any8 p xs = foldr (||) True (map p xs)

testAny fn =
  fn even []        == False &&
  fn even [1]       == False &&
  fn even [2]       == True  &&
  fn even [1,3,4,5] == True  &&
  fn even [1,3,5,7] == False

-- Ex (3)
takeWhile'      :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []  = []
takeWhile' p (x:xs)
  | p x          = x : takeWhile' p xs
  | otherwise    = []


takeWhile1,takeWhile2,takeWhile3,takeWhile4,takeWhile5
  :: (a -> Bool) -> [a] -> [a]

-- bad  - wrong results, filter
takeWhile1 _ []    = []
takeWhile1 p (x:xs)
  | p x        = x : takeWhile1 p xs
  | otherwise  = takeWhile1 p xs

-- good
takeWhile2 _ []    = []
takeWhile2 p (x:xs)
  | p x        = x : takeWhile2 p xs
  | otherwise  = []

-- bad  - delete list - :-)
takeWhile3 _ []    = []
takeWhile3 p (x:xs)
  | p x        = takeWhile3 p xs
  | otherwise  = []

-- bad - reverse take whil
takeWhile4 p = foldl (\ acc x -> if p x then x : acc else acc) []

--  still bad
takeWhile5 p xs = reverse (takeWhile4 p xs)

testTakeWhile fn =
  fn even []        == []    &&
  fn even [2]       == [2]   &&
  fn even [1]       == []    &&
  fn even [2,4,5,7] == [2,4] &&
  fn even [1,2,4,6] == []

-- Ex (4) dropwile
dropWhile',dropWhile2,dropWhile3,dropWhile4
  :: (a -> Bool) -> [a] -> [a]

-- good
dropWhile' _ []  = []
dropWhile' p (x:xs)
  | p x          = dropWhile' p xs
  | otherwise    = x:xs

-- bad  wrong results
dropWhile2 _ []  = []
dropWhile2 p (x:xs)
  | p x          = dropWhile2 p xs
  | otherwise    = xs

-- bad - filter
dropWhile3 p = foldr (\x acc -> if p x then acc else x:acc) []

-- bad  - reverse
dropWhile4 p = foldl add []
  where add [] x = if p x then [] else [x]
        add acc x = x : acc

testDropWhile fn =
  fn even []        == []   &&
  fn even [2]       == []   &&
  fn even [1]       == [1]  &&
  fn even [2,4,6,7] == [7]  &&
  fn even [1,2,4]   == [1,2,4]

-- Ex 7.3  - redfine 'map f' and 'filter p' using foldr
map'  :: (a -> b) -> [a] -> [b]
map' f  = foldr (\x xs -> f x : xs)  []

-- map f [1,2,3]
-- map f (1:(2:(3:[])))
-- f 1 : (f 2 : (f 3 : []))

-- Ex (5)
map3,map5,map6 :: (a -> b) -> [a] -> [b]

-- bad - reverses list
map3 f = foldr (\ x xs -> xs ++ [f x]) []

-- bad - concatMap
map4 :: (a -> [b]) -> [a] -> [b]
map4 f = foldr (\ x xs -> f x ++ xs) []

-- bad - reverse map
map5 f = foldl (\ xs x -> f x : xs) []

--
map6 f = foldl (\ xs x -> xs ++ [f x]) []

testMap fn =
  fn (+1) []      == []   &&
  fn (+1) [1]     == [2]  &&
  fn (+1) [1,2,3] == [2,3,4]


-- Ex (6) - filter

filter',filter1,filter2,filter3
  :: (a -> Bool) -> [a] -> [a]

filter' p = foldr (\x xs -> if p x then x:xs
                                    else xs) []
-- bad  - reverse filter
filter1 p = foldl (\ xs x -> if p x then x:xs else xs) []

-- good
filter2 p = foldr (\ x xs -> if p x then x:xs else xs) []

-- bad  - reverse filter
filter3 p = foldr (\ x xs -> if p x then xs ++ [x] else xs) []

-- doesnt compiel
-- filter4 p = foldl (\ x xs -> if p x then xs ++ [x] else xs) []

testFilter fn =
  fn even []        == []   &&
  fn even [1]       == []   &&
  fn even [2]       == [2]  &&
  fn even [1,2,3,4] == [2,4]


-- Ex 7.4 -- dec2int
-- Ex (7)
dec2int,dec2int3  :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0

-- good
dec2int3 = foldl (\ x y -> 10*x + y) 0

testDec2int fn =
  fn []         == 0 &&
  fn [0,0,0,0]  == 0 &&
  fn [2,3,4,5]  == 2345


-- Ex 7.5  sumsqreven2
-- Ex (8)

-- compose a list of functions
compose :: [a-> a] -> (a -> a)
compose  = foldr (.) id

sumsqreven   :: [Int] -> Int
sumsqreven ns  = sum (map (^2)(filter even ns))

sumsqrevne1 :: [Int] -> Int
sumsqrevne1 = sum . map (^2) . filter even

--sumsqreven3 :: [Int] -> Int
--sumsqreven3 = compose [sum, map (^2), filter even]
-- incompatiple types
-- The definitions of sumsqreven doesn't even type check

-- Ex 7.6  curry and uncurry

curry',curry'',curry3
  :: ((a, b) -> c) -> a -> b -> c

curry' f = (\x -> \y -> f(x,y))
curry'' f x y = f(x,y)

-- bad - wrong type - the flip function
curry1 :: (a -> b -> c) -> a -> b -> c
curry1 f = \ x y -> f x y

-- bad - wong type
curry2 :: a -> b -> c -> a
curry2 f = \ x y -> f

-- good
curry3 f = \ x y -> f (x, y)

-- bad - acurall uncurry
curry4 :: (a -> b -> c) -> (a, b) -> c
curry4 f = \ (x,y) -> f x y


-- Ex (10)
uncurry',uncurry''  :: (a -> b -> c) -> (a, b) -> c
uncurry' f  = \(x,y) -> f x y
uncurry'' f (x,y) = f x y


-- Ex 7.7  -- unfold
-- Ex (11)
-- ecapsulates a pattern producint a list

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
  | p x        = []
  | otherwise  = h x : unfold p h t (t x)

-- p - (a -> Bool)  true for to produce emtpy list
-- h - (a -> b)     produces head
-- t - (a -> a)    produces an arguement

int2bin   :: Int -> [Int]
int2bin n
  | n == 0   =  []
  | otherwise   =  n `mod` 2 : int2bin(n `div` 2)

int2bin1 :: Int -> [Int]
int2bin1  = unfold (== 0) (`mod` 2) (`div` 2)

-- Ex (12)
chop81     :: [Int] -> [[Int]]
chop81 xs
  | null xs   = []
  | otherwise = take 8 xs : chop81 (drop 8 xs)

chop8'   :: [Int] -> [[Int]]
chop8'  = unfold null (take 8) (drop 8)


-- Ex (12)
map1 :: (a -> b) -> [a] -> [b]
map1 f xs
  | null xs    = []
  | otherwise  = f (head xs) : map f (tail xs)

map2 :: (a -> b) -> [a] -> [b]
map2 f  = unfold null (f . head) tail

-- Ex (13)
--iterate1 :: (a -> a) -> a -> [a]
--iterate1 f
--  | False  = []
--  | otherwise  = f x : iterate f

iterate2 :: (a -> a) -> a -> [a]
iterate2 f = unfold (const False) id f


-- Ex (14)
-- given non-bottom f g h
-- f . (g .h) = (f . g) . h


-- Ex (15)
testListProperties =
  (1 : [2,3])          == ([1] ++ [2,3])  &&
  ([] ++ [2,3])        == [2,3]  &&
  (1 : ([2,3]++[4,5])) == ((1 : [2,3]) ++ [4,5]) &&
--  ([1] : [2,3]) /= [1, [2,3]] && -- bad type
  1:[] ==  [1]


-- Ex (16)  - filter, map properties
-- filter p . filter p = filter p


-- Ex (17)
--(reverse (map f xs))  == map f (reverse xs)

-- Ex (18)
-- reverse (xs ++ ys) = reverse ys ++ reverse xs)

-- Ex (19)
-- take 10 [1..]

-- Ex (20)
-- sum is a higher-order function

-- Ex (21)
-- map is an overloaded function

-- Ex (22)
-- foldr is an overloaded function

-- Ex (23)
-- take is polymorphic function

-- Ex (24) - overloaded
-- f x = x > 3

-- Ex (25)
-- take 4 (iterate (+) 1)

-- Ex (26)
-- takeWhile even [2, 4, 5, 6, 7, 8] => [2,4]

-- Ex (27)
-- zip [1, 2] ['a', 'b', 'c']
-- [(1,'a'),(2,'b')]

-- Ex (28)
-- foldr (-) 0 [1, 2, 3, 4]  => -2

-- Ex (29)
-- filter even (map (+1) [1..5])
-- [2,4,6]

-- Ex (30)
-- [f x | x <- xs, p(f x)]

-- Ex (31)
-- cExp (CNat a) (CNat b) = CNat (b a)



-- Ex 7.7  string transmitter with parity

type Bit = Int

bin2int  :: [Bit] -> Int
bin2int  = foldr (\x y -> x +2 * y) 0

-- truncate or extend to 8 bits
make8     :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)


-- Transmission

encode :: String -> [Bit]
encode = concatMap (addparity . make8 . int2bin . ord)

-- chop a string of bits into 9 bit binaries
chop9     :: [Bit] -> [[Bit]]
chop9 []   = []
chop89 bits = take 9 bits : chop9 (drop 9 bits)

decode   :: [Bit] -> String
decode = map (chr . bin2int . checkparity) . chop9

addparity     :: [Bit] -> [Bit]
addparity msg  = (parity msg) : msg

parity        :: [Bit] -> Bit
parity msg
  | odd (sum msg)  = 1
  | otherwise      = 0

checkparity   :: [Bit] -> [Bit]
checkparity (m:msg)
  | m == parity msg  = msg
  | otherwise        = error "bad message"

transmit  :: String -> String
transmit  = decode . channel . encode

channel  :: [Bit] -> [Bit]
channel = id


map10,map11,map12 :: (a -> b) -> [a] -> [b]

map10 f = foldl (\ xs x -> xs ++ [f x]) []

map11 f = foldr (\ x xs  -> xs ++ [f x]) []

map12 f  = foldr (\x xs -> f x : xs)  []


fun3 :: (a -> b) -> [b] -> a -> [b]
fun3 f  = (\ xs x -> xs ++ [f x])

fun4 :: (a -> b) -> a -> [b] -> [b]
fun4 f = (\x xs -> f x : xs)
