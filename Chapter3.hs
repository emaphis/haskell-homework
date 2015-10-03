-- | Chapter 3 from Thompsons book
-- | Basic data types

module Chapter3 where
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

-- threeDifferent -> False because  m == p

-- another definition
threeDifferent1 ::  Integer -> Integer -> Integer -> Bool
threeDifferent1 m n p = not ((m==n) || (m==p) || (n==p))

prop_3d ::  Integer -> Integer -> Integer -> Bool
prop_3d m n p = threeDifferent m n p == threeDifferent1 m n p


-- Ex 3.10  define fourEqual

fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual m n o p = (m==n) && (n==o) && (o==p)

fourEqual1 :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual1 m n o p = threeEqual m n o && p == o

prop_4e :: Integer -> Integer -> Integer -> Integer -> Bool
prop_4e m n o p = fourEqual m n o p  == fourEqual1 m n o p

-- Ex 3.12  QuickCheck definitions - see abouve
