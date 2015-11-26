-- | Homework for Lecture 3, Chapter 4
-- | Function Definition


module Homework03 where

-- Ex (0)
-- 4.1  - define function halve that splits an even lengthe list into two

halve   :: [a] -> ([a],[a])  -- mine from book
halve xs = (take hlf xs, drop hlf xs)
  where hlf = length xs `div` 2

halve2,halve3,halve5,halve6,halve8   :: [a] -> ([a],[a])
-- * b g g b b g b g

--lve1 xs = (take n xs, drop n xs)   -- bad type '/'
--where n = (length xs / 2)

halve2 xs =  -- good
  splitAt (length xs `div` 2) xs

halve3 xs =  -- good
  (take (n `div` 2) xs, drop (n `div` 2) xs)
    where n = length xs

--halve4 xs =  -- bad, malformed
--  splitAt (length xs `div` 2)

halve5 xs =  -- bad,  bad output
  (take n xs, drop (n+1) xs)
    where n = length xs `div` 2

halve6 xs =  -- good
  splitAt (div (length xs) 2) xs

--halve7 xs =  -- bad, type error
--  splitAt (length xs / 2) xs

halve8 xs =  -- good
  (take n xs, drop n xs)
    where n = length xs `div` 2

test_halve :: (Num a,  Eq a) =>
  ([a] -> ([a], [a])) -> Bool
test_halve fn =
  fn []            == ([],[]) &&  -- base case
  fn [1,2]         == ([1],[2]) &&
  fn [1,2,3,4,5,6] == ([1,2,3],[4,5,6])


-- Ex (1)
-- 4.2  - safe tail - conditional, guarded, pattern

safetailc,safetail2,safetail3,safetailg,safetail6, safetail8, safetailp
    :: [a] -> [a]

-- * g g b g b g b g

safetailc xs =
  if null xs then [] else tail xs

safetail2 [] = []
safetail2 (_:xs) = xs

safetail3 (_:xs)  -- bad, non exhaustive pattern
  | null xs = []
  | otherwise = tail xs

safetailg xs | null xs    = []
             | otherwise  = tail xs

--safetail5 xs = tail xs -- bad, overlapped matches
-- safetail5 [] = []

safetail6 [] = []
safetail6 xs = tail xs

--safetail7 [x] = [x]  --bad
--safetail7 (__:xs) = xs

safetail8
  = \ xs ->
      case xs of
          [] -> []
          (_ : xs) -> xs

safetailp (_:xs)  = xs
safetailp []      = []

test_tail :: (Num a, Eq a) => ([a] -> [a]) -> Bool
test_tail fn =
  fn []       == []  &&
  fn [1]      == []  &&
  fn [1,2,3]  == [2,3]


-- Ex (2)
-- 4.3  - define 'or' '||'  four ways using pattern matching

(||*),(||^),(||@),(||#),(||$),(||&),(||-)
  :: Bool -> Bool -> Bool

-- * g g b
False ||* False  = False
_     ||* _      = True

False ||^ b  = b
True  ||^ _  = True

b ||@ c | b==c       = True  -- False
        | otherwise  = False

b ||# c | b==c       = b
        | otherwise  = True

b ||$ False = b
_ ||$ True  = True

b ||& c
  | b==c      = c
  | otherwise = True

-- b ||& True = b     -- bad non exhaustive pattenr
-- _ ||& True = True

False ||- False = False
True  ||- True  = True
True  ||- False = True
False ||- True  = True

test_or :: (Bool -> Bool -> Bool) -> Bool
test_or fn =
  fn True  True   &&
  fn True  False  &&
  fn False True   &&
  not (fn False False)


-- Ex (3)
-- 4.4  redefine '&&' using conditional expressions

(&&~),(&&!),(&&@),(&&#),(&&$),(&&%),(&&*),(&&&)  :: Bool -> Bool -> Bool

True &&~ True  = True
_    &&~ _     = False

a &&! b = if a then if b then True else False else False

a &&@ b =   -- bad
  if not (a) then not (b) else True

-- a &&# b = if a then b  -- bad form

a &&# b =   -- bad
  if a then if b then False else True else False

a &&$ b = if a then b else False

a &&% b = if b then a else False

(&&*) a b = if a==b then a else False  -- mine

(&&&) a b  = if a then
               if b then True else False
             else False

test_and  :: (Bool -> Bool -> Bool) -> Bool
test_and fn =
  fn True  True        &&
  not (fn False True)  &&
  not (fn True  False) &&
  not (fn False False)


-- 4.5 now define or using conditionals

(||%), (||!)   :: Bool -> Bool -> Bool

(||%) a b = if (a == b) then a else True

(||!) a b = if a then b else False

-- Ex (4)
-- 4.6  - lambda curried mult x y z = x * y * z

mult, mult3 :: Int -> (Int -> (Int -> Int))

-- mult1 x y z = \x -> (\y -> (\z -> x * y *z))

-- mult2 = \x -> (x * \y -> (y * \z -> z))

mult3 = \x -> (\y -> (\z -> x * y * z))

-- mult4 = (((\x -> \y -> \z) -> x * y) * z)

mult = \x -> \y -> \z -> x * y * z


-- Ex (5)  -- f x g y means:
--            ((f x) g) y

f :: Int -> (Int -> Int) -> Int -> Int
f x g y = x + g y

g :: Int -> Int
g y  = y + 1

h :: Int -> Int -> Int
h x y = ((f x) g) y


-- Ex (6)  -  f :: (a -> a) -> a
--   takes a function as an argument


-- Ex (7)  - Chose the corredt implementations for the function 'remove'

-- take a number 'n' and a list and remove the element at that position
remove :: Int -> [a] -> [a]
-- remove n xs = take n xs ++ drop n xs
-- remove n xs = drop n xs ++ take n xs
-- remove n xs = take (n+1) xs ++ drop n xs  -- not good
remove n xs = take n xs ++ drop (n+1) xs  -- good

test_remove fn =
  fn 0 [1,2,3,4]  == [2,3,4]


-- Ex (8)  --

-- copy the x'th element
funct   :: Int -> [a] -> [a]
funct x xs = take (x+1) xs ++ drop x xs



-- Lab 03  - Types - How to recognize them


-- Ex (0)
e0 = [False, True, False, True] :: [Bool]


-- Ex (1)

e1 = [[1,2], [3,4]]  :: Num t => [[t]]

-- [[Integer]]
-- [Int]
-- Num t => [[t]]

-- Ex (2)  -- may be trouble

e2 = [[[1, 2, 3]], [[3, 4, 5]]] :: [[[Integer]]]

-- [[[Integer]]]
-- [[[Int]]]
-- Num t => [[[t]]]


-- Ex (3)
e3 :: Num a => a -> a
e3 x = x * 2


-- Ex (4)
e4 :: (a,b) -> a
e4 (x, y) = x


-- Ex (5)

e5 :: (a, b, c) -> c
e5 (x, y, z) = z


-- Ex (6)

e6 :: Num a => a -> a -> a
e6 x y = x * y


-- Ex (7)

e7 :: (a, b) -> (b, a)
e7 (x, y) = (y, x)


-- Ex (8)

e8 :: a -> b -> (b, a)
e8 x y = (y, x)


-- Ex (9)

e9 :: [t] -> (t, Bool)
e9 [x, y] = (x, True)


-- Ex (10)

e10 :: (a,a) -> [a]
e10 (x, y) = [x, y]


-- Ex (11)

e11 :: (Char, Bool)
e11 = ('\a', True)


-- Ex (12)

e12 :: [(Char, Int)]
e12 = [('\a', 1)]


-- Ex (13)

e13 :: Int -> Int -> Int
e13 x y = x + y * y


-- Ex (14)

e14 :: ([Char], [Float])
e14 = ("Haskell", [3.1, 3,14, 3.145])


-- Ex (15)

e15 :: [a] -> [b] -> (a, b)
e15 xs ys = (head xs, head ys)
