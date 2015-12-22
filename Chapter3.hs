-- | Chapter 3 from Thompsons book
-- | Basic data types

module Chapter3 where
import Prelude hiding (max, min)
import Test.QuickCheck

-- Booleans - the Bool type

-- Exclusive or
exOr :: Bool -> Bool -> Bool
exOr x y = (x || y) && not (x && y)

--  /= is the same funtion as exOr

-- Literals and definitons - pattern matching
myNot :: Bool -> Bool
myNot True  = False
myNot False = True

prop_myNot :: Bool -> Bool
prop_myNot x =
  not x == myNot x

exOr1 :: Bool -> Bool -> Bool
exOr1 True  x = not x
exOr1 False x = x

prop_exOrs :: Bool -> Bool -> Bool
prop_exOrs x y =
  exOr x y == exOr1 x y

prop_exOr2 :: Bool -> Bool -> Bool
prop_exOr2 x y =
  exOr x y == (x /= y)


-- Ex 3.1 Another exOr

exOr3 :: Bool -> Bool -> Bool
exOr3 x y = (x && not y) || (not x && y)

prop_exOr3 :: Bool -> Bool -> Bool
prop_exOr3 x y = exOr x y == exOr3 x y

-- Ex 3.3  exOr using literals
exOr4 :: Bool -> Bool -> Bool
exOr4 True  True  = False
exOr4 False x     = x
exOr4 True  False = True

prop_exOr4 :: Bool -> Bool -> Bool
prop_exOr4 x y =
  exOr4 x y == (x /= y)

-- Ex 3.4 a definition of && and ||
(&&-) :: Bool -> Bool -> Bool
(&&-) False _  = False
(&&-) True  x  = x

-- (&&-) a b = not ( not a || not b)

prop_and :: Bool -> Bool -> Bool
prop_and x y = (x &&- y) == (x && y)

(||-) :: Bool -> Bool -> Bool
(||-) True  _  = True
(||-) False x  = x

prop_or :: Bool -> Bool -> Bool
prop_or x y = (x ||- y) == (x || y)

-- Ex 3.5 - two deffinitions of nAnd

nAnd :: Bool -> Bool -> Bool
nAnd x y = not (x && y)

nAnd1 :: Bool -> Bool -> Bool
nAnd1 True True = False
nAnd1 _ _ = True

prop_nand :: Bool -> Bool -> Bool
prop_nand x y = nAnd x y == nAnd1 x y


-- Ex 3.7 quick check properties - see above


-----------------
-- 3.2 The integers Integer and Int

-- + * ^ - div mod abs negate
-- > >= == /= <= <

threeEqual :: Integer -> Integer -> Integer -> Bool
threeEqual m n p = (m==n) && (n==p)


-- Ex 3.8
mystery :: Integer -> Integer -> Integer -> Bool
mystery m n p = not ((m==n) && (n==p))

--mystery m n p = not (threeEqual m n p)
-- returns true if any of three numbers is equal to the others

-- Ex 3.9 - True if all three numbers are different

threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent m n p = (m /= n) && (n /= p) && (m /= p)

-- threeDifferent 3 4 3 -> False because  m == p

-- another definition
threeDifferent1 ::  Integer -> Integer -> Integer -> Bool
threeDifferent1 m n p = not ((m==n) || (n==p) || (n==p))

prop_3d ::  Integer -> Integer -> Integer -> Bool
prop_3d m n p = threeDifferent m n p == threeDifferent1 m n p


-- Ex 3.10  define fourEqual

fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual m n o p = (m==n) && (n==o) && (o==p)

fourEqual1 :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual1 m n o p = threeEqual m n o && p == o

prop_4e :: Integer -> Integer -> Integer -> Integer -> Bool
prop_4e m n o p = fourEqual m n o p  == fourEqual1 m n o p

-- Ex 3.12  QuickCheck definitions - see above


-- 3.4 Guards

max :: Integer -> Integer -> Integer
max x y
  | x >= y     = x
  | otherwise  = y

maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z
  | x >= y && x >= z   = x
  | y >= z             = y
  | otherwise          = z

prop_maxThree1, prop_maxThree2 :: Integer -> Integer -> Integer -> Bool
prop_maxThree1 x y z =
  x <= maxThree x y z || y <= maxThree x y z || z <= maxThree x y z
prop_maxThree2 x y z =
  x <= maxThree x y z && y <= maxThree x y z && z <= maxThree x y z


-- Conditional expression
-- if condition the a else b

-- max using a condtional
max' :: Integer -> Integer -> Integer
max' x y =
  if x >= y then x else y

prop_compareMax :: Integer -> Integer -> Bool
prop_compareMax x y =
  max x y == max' x y

-- properties of max
-- The maximum of x and y will be greater than or equal to both x and y.
-- The maximum of x and y will actually be equal to one (or both) of x and y.
prop_maxl, prop_max2 :: Integer -> Integer -> Bool
prop_maxl x y =
  x <= max x y && y <= max x y
prop_max2 x y =
  x == max x y || y == max x y

  -- Ex 3.14 - Give a definition of min and minThree
min :: Int -> Int -> Int
min x y
  | x <= y    = x
  | otherwise = y

prop_min1, prop_min2 :: Int -> Int -> Bool
prop_min1 x y =
  x >= min x y && x >= min x y
prop_min2 x y =
  x == min x y || y == min x y

minThree :: Int -> Int -> Int -> Int
minThree x y z
  | x <= y && x <= z   = x
  | y <= z             = y
  | otherwise          = z

prop_minThree1, prop_minThree2 :: Int -> Int -> Int -> Bool
prop_minThree1 x y z =
  x >= minThree x y z || y >= minThree x y z || z >= minThree x y z

prop_minThree2 x y z =
  x >= minThree x y z && y >= minThree x y z && z >= minThree x y z



-- 3.5 Characters and Strings
{-
  tab              '\t'
  newline          '\n'
  backslash (\)    '\\'
  single quote (') '\''
  double quote (") '\"'

  fromEnum :: Char -> Int
  toEnum :: Int -> Char
-}

offset :: Int
offset = fromEnum 'A' - fromEnum 'a'

toUpper :: Char -> Char
toUpper ch = toEnum (fromEnum ch + offset)

isDigit :: Char -> Bool
isDigit ch = ('0' <= ch) && (ch <= '9')


-- Ex 3.16  toUpper that ignores non small char's

toUpper1 :: Char -> Char
toUpper1 ch =
  if isSmall ch then toUpper ch else ch

isSmall :: Char -> Bool
isSmall ch = ('a' <= ch) && ('z' <= ch)

-- Ex 3.17 define a function that converts a Char to an Int
-- non-numbers should be 0

charToNum :: Char -> Int
charToNum ch =
  if isDigit ch then (fromEnum ch - nOffset) else 0

nOffset :: Int
nOffset = fromEnum '0'

-- String
str1, str2, str3, str4 :: String

str1 = "baboon"
str2 = "\99a\116"
str3 = "gorilla\nhippo\nibex"
str4 = "I\t23\t456"

ptstr1, ptstr2, ptstr3, ptstr4 :: IO ()
ptstr1 = putStr str1
ptstr2 = putStr str2
ptstr3 = putStr str3
ptstr4 = putStr str4
{-
  putStr :: String -> 10 ()
  show - convert value to a String
  read - convert from a String to value
-}

-- Ex 3.18 print out three lines
onThreeLines :: String -> String -> String -> String
onThreeLines s1 s2 s3 =
  s1 ++ "\n" ++ s2 ++ "\n" ++ s3 ++ "\n"

-- Ex 3.19 Romean digints
romanDigit :: Char -> String
romanDigit num
  | num == '0'  = ""
  | num == '1'  = "I"
  | num == '2'  = "II"
  | num == '3'  = "III"
  | num == '4'  = "IV"
  | num == '5'  = "V"
  | num == '6'  = "VI"
  | num == '7'  = "VII"
  | num == '8'  = "VIII"
  | num == '9'  = "IX"
  | otherwise   = ""


-- 3.6 Floating-point numbers: Float

flt1, flt2, flt3, flt4 :: Float
flt1 = 0.31426
flt2 = -23.12
flt3 = 567.347
flt4 = 4523.0

-- fromInteger :: Integer->Float
-- FromIntegral :: Int -> Float
-- ceiling, floor round :: Float -> Integer

-- Ex 3.20 - average of three fucntion
averageThree :: Integer -> Integer -> Integer -> Float
averageThree n m o = fromInteger(n + m + o) / 3.0
