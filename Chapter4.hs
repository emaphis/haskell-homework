-- | Chapter 4 - Desinging ans writing programs

module Chapter4 where
import Test.QuickCheck (quickCheck)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.HUnit
import PicturesSVG

maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z = max ( max x y) z

-- testing section 4.8
testMaxl = TestCase (assertEqual "for: maxThree 6 4 1" 6 (maxThree 6 4 1))
testMax2 = TestCase (assertEqual "for: maxThree 6 6 6" 6 (maxThree 6 6 6))
testMax3 = TestCase (assertEqual "for: maxThree 2 6 6" 6 (maxThree 2 6 6))
testMax4 = TestCase (assertEqual "for: maxThree 2 2 6" 6 (maxThree 2 2 6))
testMax5 = TestCase (assertEqual "for: MaxThree 6 6 2" 6 (maxThree 6 6 2))

testsMax = TestList [testMaxl, testMax2, testMax3, testMax4, testMax5]

-- 4.1 Middle number example

middleNumber :: Integer -> Integer -> Integer -> Integer
middleNumber x y z
  | between y x z   = x
  | between x y z   = y
  | otherwise       = z

between :: Integer -> Integer -> Integer -> Bool
between x y z =
  (x >= y && y >= z) || (x <= y && y <= z)


-- Exercises

-- Ex 4.1 define maxFour

maxFour :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour w x y z
  | w >= x && w >= y && w >= z  = w
  | x >= y && x >= z            = x
  | y >= z                      = y
  | otherwise                   = z

maxFour1 :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour1 w x y z = max (max (max w x) y) z

maxFour2 :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour2 w x y z = max (maxThree w x y) z

prop_maxFour :: Integer -> Integer -> Integer -> Integer -> Bool
prop_maxFour w x y z =
  maxFour w x y z == maxFour1 w x y z

prop_maxFour1 :: Integer -> Integer -> Integer -> Integer -> Bool
prop_maxFour1 w x y z =
  maxFour w x y z == maxFour2 w x y z


-- Ex 4.2  middle number using weak asending order

middleNumber1 :: Integer -> Integer -> Integer -> Integer
middleNumber1 x y z
  | weakAscendingOrder x y z || weakAscendingOrder z y x = y
  | weakAscendingOrder y x z || weakAscendingOrder z x y = x
  | otherwise     = z

weakAscendingOrder :: Integer -> Integer -> Integer -> Bool
weakAscendingOrder m n p = (p >= n) && (n >= m)

prop_middleNumber :: Integer -> Integer -> Integer -> Bool
prop_middleNumber x y z =
  middleNumber x y z == middleNumber1 x y z


-- Ex 4.3  how many equal

-- From section 3.2
threeEqual :: Integer -> Integer -> Integer -> Bool
threeEqual m n p = (m==n) && (n==p)

-- from Ex 3.9
threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent m n p = (m /= n) && (n /= p) && (m /= p)


howManyEqual :: Integer -> Integer -> Integer -> Integer
howManyEqual x y z
  |  threeEqual x y z      = 3
  |  threeDifferent x y z  = 1
  |  otherwise             = 2


-- Ex 4.4  - How many equal four

howManyEqual4 :: Integer -> Integer -> Integer -> Integer -> Integer
howManyEqual4 w x y z
  | fourEqual w x y z       = 4
  | onlyThreeEqual w x y z  = 3
  | fourDifferent w x y z   = 1
  | otherwise               = 2

fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual w x y z =
    threeEqual w x y && w == z

onlyThreeEqual :: Integer -> Integer -> Integer -> Integer -> Bool
onlyThreeEqual w x y z =
    threeEqual w x y || threeEqual w x z || threeEqual w y z || threeEqual x y z


fourDifferent :: Integer -> Integer -> Integer -> Integer -> Bool
fourDifferent w x y z =
    threeDifferent w x y && z /= w && z /= x && z /= y



-- 4.2 Solving a problem ins steps: local definitions

fourPics0 :: Picture -> Picture
fourPics0 pic =
  left `beside` right
  where
    left = pic `above` invertColour pic
    right = invertColour (flipV pic) `above` flipV pic

fourPics1 :: Picture -> Picture
fourPics1 pic =
  left `beside` right
  where
    left = pic `above` invertColour pic
    right = invertColour flipped `above` flipped
    flipped = flipV pic

fourPics2 :: Picture -> Picture
fourPics2 pic =
  left `beside` right
  where
    left = pic `above` invertColour pic
    right = invertColour (flipV left)


fourPics3 :: Picture -> Picture
fourPics3 pic =
  left `beside` right
  where
    stack p = p `above` invertColour p
    left    = stack pic
    right   = stack (invertColour (flipV pic))


-- area of trinangle

triArea :: Float -> Float -> Float -> Float
triArea a b c
  | possible  = sqrt(s*(s-a)*(s-b)*(s-c))
  | otherwise = 0
  where
    s = (a+b+c)/2
    possible =  greaterThanZero && lengthOfSide
      where
        greaterThanZero = (a > 0) && (b > 0) && (c > 0)
        lengthOfSide = (a < b+c) && (b < a+c) && (c < a+b)

sumSquares :: Integer -> Integer -> Integer
sumSquares n m
  = sqN + sqM
  where
    sqN = n*n
    sqM = m*m


-- Scopes

isOdd, isEven :: Int -> Bool

isOdd n
  | n<=0      = False
  | otherwise = isEven (n-1)

isEven n
  | n<0       = False
  | n==0      = True
  | otherwise = isOdd (n-1)


maxsq :: Int -> Int -> Int
maxsq x y
  | sqx > sqy     = sqx
  | otherwise     = sqy
  where
    sqx  = sq x
    sqy  = sq y
    sq :: Int -> Int
    sq z = z*x


-- Exercises

-- Ex 4.5 yet two more ways to define fourPics

-- Ex 4.6
{-
fourPics :: Picture -> Picture

fourPics pic =
  top `above` bottom
   where
    top = ...:
    bottom ...
-}

fourPics4 :: Picture -> Picture
fourPics4 pic =
  top `above` bottom
  where
    top    = pic `beside` invertColour (flipV pic)
    bottom = invertColour pic `beside` flipV pic

fourPics5 :: Picture -> Picture
fourPics5 pic =
  top `above` bottom
   where
    top    = pic `beside` invertColour flipped
    bottom = invertColour pic `beside` flipped
    flipped = flipV pic

fourPics6 :: Picture -> Picture
fourPics6 pic =
  top `above` bottom
  where
    top    = pic `beside` invertColour (flipV pic)
    bottom = invertColour top

-- Exercises

-- Ex 4.7
-- TODO:

-- Ex 4.8 Define prossible values -- see above.

possible :: Integer -> Integer -> Integer -> Bool
possible a b c
  =  greaterThanZero && lengthOfSide
  where
    greaterThanZero = (a > 0) && (b > 0) && (c > 0)
    lengthOfSide = (a < b+c) && (b < a+c) && (c < a+b)


-- Ex 4.9 - define maxThreeOccurs

maxThreeOccurs :: Int -> Int -> Int -> (Int,Int)
maxThreeOccurs m n o =
  (max3e, numOccurs max3e)
  where
    max3e = max m (max n o)
    numOccurs mx
      | allFourEqual mx    = 3
      | anyTwoEqual mx     = 2
      | otherwise          = 1
      where
        allFourEqual w  =
          (w==m) && (w==n) && (w==o)
        anyTwoEqual w =
          threeEqual w m n || threeEqual w n o || threeEqual w m o
        threeEqual w x y = (w==x) && (w==y)



-- $.3 Defining types for ourselves: enumerated types.

-- Rock Paper Sissors

data Move = Rock | Paper | Scissors
          deriving (Eq)

-- Showing Moves in an abbreviated form.

instance Show Move where
      show Rock = "r"
      show Paper = "p"
      show Scissors = "s"

-- For QuickCheck to work over the Move type.

instance Arbitrary Move where
  arbitrary     = elements [Rock, Paper, Scissors]

-- Calculating the Move to beat or lose against the
-- argument Move.

beat,lose :: Move -> Move
beat Rock     = Paper
beat Paper    = Scissors
beat Scissors = Rock

lose Rock     = Scissors
lose Paper    = Rock
lose Scissors = Paper


-- Exercises

-- Ex 4.11  Result datatype

data Result = Win | Lose | Draw
            deriving (Show,Eq)

-- Ex 4.12  outcme function

outcome :: Move -> Move -> Result
outcome mv1 mv2
  | mv1 == beat mv2  = Win
  | mv1 == lose mv2  = Lose
  | otherwise        = Draw

-- Ex 4.13 a Win vs Lose property
-- win . loose and lose . win should be an ID funtion one move
prop_winLose :: Move -> Bool
prop_winLose move =
  beat (lose move) == move
--  beat (lose move) == lose (beat move)

-- Ex 4.14 quickCheck property to test outcome
-- if move1 /= move2 then  (move1 beat move2) == (move2 lose move1)
prop_outcome :: Move -> Move -> Bool
prop_outcome mv1 mv2
  | outcome mv1 mv2 == Win  = outcome mv2 mv1 == Lose
  | outcome mv1 mv2 == Lose = outcome mv2 mv1 == Win
  | outcome mv1 mv2 == Draw = outcome mv2 mv1 == Draw
  | otherwise               = False
-- is this a tautology?


-- Standard Types

-- Exercises

-- Ex 4.15 Season type and function

data Season = Spring | Summer | Fall | Winter
            deriving (Eq,Show,Ord)

data Temp = Cold | Hot
          deriving (Eq,Show,Ord)

temperature :: Season -> Temp
temperature sz
  | sz == Spring = Cold
  | sz == Summer = Hot
  | sz == Fall   = Hot
  | otherwise    = Cold


-- Ex 4.16  month data type and function
data Month = January | February | March |
             April | May | June |
             July | August | September |
             October | November | December  
             deriving (Eq, Show, Ord)

monthToSeason :: Month -> Season
monthToSeason mth
  | mth == March || mth == April || mth == May            = Spring
  | mth == June || mth == July || mth == August           = Summer
  | mth == September || mth == October || mth == November = Fall
  | otherwise                                             = Winter



-- 4.4  Recursion -- Oh, boy!

-- Factorials

fac :: Integer -> Integer
fac n
  | n==0      = 1
  | n>0       = fac (n-1) * n
  | otherwise = error "'fac' only defined on natural numbers"


-- Exercises

-- Ex 4.17 RangeProduct

rangeProduct :: Integer -> Integer -> Integer
rangeProduct n m
  | m == n      = n
  | m > n       = m * rangeProduct n (m-1)
  | otherwise   = 0

--- Ex 4.18  fac implemented with rangeProduct
fac1 :: Integer -> Integer
fac1 n = rangeProduct 1 n

{- --templase
fun :: Integer -> a
fun n
  | n==0 = _fun1
  | n>0  = _fun2 (fun (n-1))
-}


-- Example:

-- powers of 2
power2 :: Integer -> Integer
power2 n
  | n==0   = 1
  | n>0    = 2 * power2 (n-1)


-- sum of factorials
sumFacs :: Integer -> Integer
sumFacs n
  | n==0   = 1
  | n>0    = sumFacs (n-1) + fac n


-- sum of some functions f
sumFun :: (Integer -> Integer) -> Integer -> Integer
sumFun f n
  | n==0    = f 0
  | n>0     = sumFun f (n-1) + f n

sumFacs' :: Integer -> Integer
sumFacs' n = sumFun fac n


-- number of regions formed by cuts:

regions :: Integer -> Integer
regions n
  | n==0  = 1
  | n>0   = regions (n-1) + n


-- Exercises:

-- Ex 4.19 - define multiplication

mult :: Integer -> Integer -> Integer
mult n m
  | m==0   = 0
  | n>0    = n + mult n (m-1)


-- Ex 4.20 integer square root
intSqrRoot :: Integer -> Integer
intSqrRoot n = loop n n
  where
    loop n m
      | n*n <= m    = n
      | otherwise   = loop (n-1) m

-- Ex 4.21 maximum of functions calls

maxFun :: (Integer -> Integer) -> Integer -> Integer
maxFun f n
  | n==0    = f 0
  | n>0     = max (f n) (maxFun f (n-1))

-- a testing function - maxFun fn 100 => 44
fn :: Integer -> Integer
fn 0 = 0
fn 1 = 44
fn 2 = 17
fn _ = 0


-- Ex 4.22 function that takes a funtion and returns true
-- if given funtion returns 0

fun422 :: (Integer -> Integer) -> Integer -> Bool
fun422 f n
  | n==0   = False
  | n>0    = if f n == 0
             then True
             else fun422 f (n-1)

-- Ex 4.23 define regions in terms of sumFun
regions2 :: Integer -> Integer
regions2 n = sumFun id' n + 1
  where id' a = a
--regions2 n = sumFun (+0) n + 1

-- Ex 4.24 3d regions - cake cuting algorithm
regions3d :: Integer -> Integer
regions3d n
  | n==0   = 1
  | n>0    = regions3d (n-1) + regions (n-1)


-- $.6 pictures

blackSquares :: Integer -> Picture
blackSquares n
  | n<=1       = black
  | otherwise  = black `beside` blackSquares (n-1)

whiteSquares :: Integer -> Picture
whiteSquares n
  | n<=1       = white
  | otherwise  = white `beside` whiteSquares (n-1)


blackWhite :: Integer -> Picture
blackWhite n
  | n<=1       = black
  | otherwise  = black `beside` whiteBlack (n-1)

whiteBlack :: Integer -> Picture
whiteBlack n
  | n<=1       = white
  | otherwise  = white `beside` blackWhite (n-1)


blackChess :: Integer -> Integer -> Picture
blackChess n m
  | n<=1       = blackWhite m
  | otherwise  = blackWhite m `above` whiteChess (n-1) m

whiteChess :: Integer -> Integer -> Picture
whiteChess n m
  | n<=1       = whiteBlack m
  | otherwise  = whiteBlack m `above` blackChess (n-1) m

chess :: Integer -> Picture
chess n = blackChess n n


-- Exercises

-- 4.25 - complete whiteBlack whiteChess - Done

-- 4.36 column of pictures

column :: Picture -> Integer -> Picture
column pic n
  | n<=1       = pic
  | otherwise  = pic `above` column pic (n-1)


-- 4.27 black diagonal

blackDiagonal :: Integer -> Picture
blackDiagonal num = loop num num
  where
    loop n k
      | n<=1       = rowBW k 1 
      | otherwise  = rowBW k n `above` loop (n-1) k

rowBW :: Integer -> Integer -> Picture
rowBW n m
  | n<=1       = blackOrWhite m 1
  | otherwise  = blackOrWhite m n `beside` rowBW (n-1) m
  where
    blackOrWhite m n
      | n==m       = black
      | otherwise  = white


-- Ex 4.28  reverse black diagonl

revBlackDiagonal :: Integer -> Picture
revBlackDiagonal num = loop num num
  where
    loop n k
      | n<=1      = rowBW k k
      | otherwise = rowBW k (k-n+1) `above` loop (n-1) k


-- Ex 4.28  -- double diagonal

doubleDiagonal :: Integer -> Picture
doubleDiagonal num = loop num num
  where
    loop n k
      | n<=1       = rowBW2 k 1 k
      | otherwise  = rowBW2 k n (k-n+1) `above` loop (n-1) k


rowBW2 :: Integer -> Integer -> Integer -> Picture
rowBW2 n m o
  | n<=1       = blackOrWhite 1 m o
  | otherwise  = blackOrWhite n m o `beside` rowBW2 (n-1) m o
  where
    blackOrWhite n m o
      | n==m       = black
      | n==o       = black
      | otherwise  = white


-- Ex 4.30  - direct recursive chessBoard

chessBoard :: Integer -> Picture
chessBoard num = loop num num
  where
    loop n k 
      | n<=1       = pickRow 1 
      | otherwise  = pickRow n `above` loop (n-1) k
      where
        pickRow n1
          | (n1 `mod` 2) == 0  = blackWhite k
          | otherwise          = whiteBlack k


-- 4.7 General forms of recursion

-- Examples

-- Fibonacci
fib :: Integer -> Integer
fib n
  | n==0  = 0
  | n==1  = 1
  | n>1   = fib (n-2) + fib (n-1)


-- remander and divide
remainder :: Integer -> Integer -> Integer
divide :: Integer -> Integer -> Integer

remainder m n
  | m<n       = m
  | otherwise = remainder (m-n) n
                
divide m n
  | m<n       = 0
  | otherwise = 1 + divide (m-n) n

factorInRange :: Integer -> Integer -> Bool
factorInRange k n
  | k >= n        = False
  | mod n k == 0  = True
  | otherwise     = factorInRange (k+1) n

prime :: Integer -> Bool
prime n = (n>1) && not (factorInRange 2 n)


-- Exercises

-- Ex 4.31 greatest common factor

-- Euclids algorithm
gcd' :: Integer -> Integer -> Integer
gcd' m n
  | mod m n == 0  = n
  | otherwise     = gcd' n  (mod m n)

-- gcd' 468 24  => 12
-- gcd' 135 19  => 1

-- Dijkstra's Algorithm
gcd2 :: Integer -> Integer -> Integer
gcd2 m n
  | m==n       = m
  | m>n        = gcd2 (m-n) n
  | otherwise  = gcd2 m (n-m)


-- Ex 4.32  - power of two

powerOfTwo :: Integer -> Integer
powerOfTwo n
  | n==0           = 1
  | n==1           = 2
  | mod n 2 == 0   = powerOfTwo (div n 2) *
                     powerOfTwo (div n 2)
  | otherwise      = 2 * powerOfTwo (div n 2) *
                     powerOfTwo (div n 2)

-- 4.8 Program testing
mysteryMax :: Integer -> Integer -> Integer -> Integer
mysteryMax x y z
  | x > y && x > z =x
  | y > x && y > z =y
  | otherwise = z

testMysteryMaxl = TestCase (assertEqual "for: mysteryMax 6 4 1" 6 (mysteryMax 6 4 1))
testMysteryMax2 = TestCase (assertEqual "for: mysteryMax 6 6 6" 6 (mysteryMax 6 6 6))
testMysteryMax3 = TestCase (assertEqual "for: mysteryMax 2 6 6" 6 (mysteryMax 2 6 6))
testMysteryMax4 = TestCase (assertEqual "for: mysteryMax 2 2 6" 6 (mysteryMax 2 2 6))
testMysteryMax5 = TestCase (assertEqual "for: mysteryMax 6 6 2" 6 (mysteryMax 6 6 2))

testsMysterMax = TestList [testMysteryMaxl, testMysteryMax2, testMysteryMax3,
                           testMysteryMax4, testMysteryMax5]

fact :: Int -> Int
fact n
  | n>1        = n * fact (n-1)
  | otherwise  = 1

-- fails on 17
prop_fact n =
  fact n > 0


-- Exercises

-- 4.32 test allEqual

allEqual :: Integer -> Integer -> Integer -> Bool
allEqual m n p = (m==n) && (n==p)

testAllEqual1 = TestCase (assertEqual "allEqual 5 5 5" True (allEqual 5 5 5))
testAllEqual2 = TestCase (assertEqual "allEqual 5 5 3" False (allEqual 5 5 3))
testAllEqual3 = TestCase (assertEqual "allEqual 3 5 3" False (allEqual 3 5 3))
testAllEqual4 = TestCase (assertEqual "allEqual 3 5 5" False (allEqual 3 5 5))
testAllEqual5 = TestCase (assertEqual "allEqual 2 3 5" False (allEqual 2 3 5))
testAllEqual6 = TestCase (assertEqual "allEqual -5 5 0" False (allEqual (-5) 5 0))

testsAllEqual = TestList [testAllEqual1 , testAllEqual2, testAllEqual3, testAllEqual4,
                          testAllEqual5, testAllEqual6]

-- quickCheck -- compare against "trusted" threeEqual
prop_allEqual m n p =
  allEqual m n p == threeEqual m n p

-- allEqual should be different for allDifferent:
prop_allEqual_not_threeDifferent m n p =
  not (allEqual m n p && threeDifferent m n p)

-- Ex 4.34
solution :: Integer -> Integer -> Integer -> Bool
solution m n p = ((m+n+p)==3*p)

-- pretend allEqual

testSolution1 = TestCase (assertEqual "solution1 5 5 5" True  (solution 5 5 5))
testSolution2 = TestCase (assertEqual "solution1 5 5 3" False (solution 5 5 3))
testSolution3 = TestCase (assertEqual "solution1 3 5 3" False (solution 3 5 3))
testSolution4 = TestCase (assertEqual "solution1 3 5 5" False (solution 3 5 5))
testSolution5 = TestCase (assertEqual "solution1 2 3 5" False (solution 2 3 5))

-- this should fail:
testSolution6 = TestCase (assertEqual "solution1 -5 5 0" False (solution (-5) 5 0))

testsAllSolution = TestList [testSolution1 , testSolution2, testSolution3, testSolution4,
                          testSolution5, testSolution6]
-- all tests passed, except when any are negative

-- quicCheck against "trusted"  threeEqual  -- this should fail
prop_solution :: Integer -> Integer -> Integer -> Bool
prop_solution  m n o =
  solution m n o  ==  threeEqual m n o

-- failed:  -4 6 1, 5 -1 2, 2 -6 -2, -12 2 -5, 3 7 5,
-- but many runs of quickCheck passed, oops


-- Ex 4.35 test all different
allDifferent :: Integer -> Integer -> Integer -> Bool
allDifferent m n p = (m /= n) && (n /= p) && (m /= p)

testAllDifferent1 = TestCase (assertEqual "allDifferent 5 5 5" False (allDifferent 5 5 5))
testAllDifferent2 = TestCase (assertEqual "allDifferent 5 5 3" False (allDifferent 5 5 3))
testAllDifferent3 = TestCase (assertEqual "allDifferent 3 5 3" False (allDifferent 3 5 3))
testAllDifferent4 = TestCase (assertEqual "allDifferent 3 5 5" False (allDifferent 3 5 5))
testAllDifferent5 = TestCase (assertEqual "allDifferent 2 3 5" True  (allDifferent 2 3 5))
testAllDifferent6 = TestCase (assertEqual "allDifferent -5 5 0" True  (allDifferent (-5) 5 0))

testsAllDifferent = TestList [testAllDifferent1 , testAllDifferent2, testAllDifferent3,
                          testAllDifferent4,  testAllDifferent5, testAllDifferent6]

-- quickCheck - test against "trusted" threeDifferent
prop_allDifferent :: Integer -> Integer -> Integer -> Bool
prop_allDifferent m n o =
  allDifferent m n o  ==  threeDifferent m n o


-- Ex 4.36  test attempt

attempt :: Integer -> Integer -> Integer -> Bool
attempt m n p = (m/=n) && (n/=p)

testAttempt1 = TestCase (assertEqual "attempt 5 5 5" False (attempt 5 5 5))
testAttempt2 = TestCase (assertEqual "attempt 5 5 3" False (attempt 5 5 3))
testAttempt3 = TestCase (assertEqual "attempt 3 5 3" False (attempt 3 5 3)) -- fails
testAttempt4 = TestCase (assertEqual "attempt 3 5 5" False (attempt 3 5 5))
testAttempt5 = TestCase (assertEqual "attempt 2 3 5" True  (attempt 2 3 5))
testAttempt6 = TestCase (assertEqual "attempt -5 5 0" True  (attempt (-5) 5 0))


testsAttempt = TestList [testAttempt1 , testAttempt2, testAttempt3, testAttempt4,
                          testAttempt5, testAttempt6]

-- Faulty function, it doesn't catch the 3 5 3 case since it doesn't test m /= p case


-- Ex 4.38  - test power of two: from ex 4.32

powerOfTwo' :: Integer -> Integer
powerOfTwo' n
  | n==0           = 1
  | n==1           = 2
  | mod n 2 == 0   = powerOfTwo' (div n 2) *
                     powerOfTwo' (div n 2)
  | otherwise      = 2 * powerOfTwo (div n 2) *
                     powerOfTwo' (div n 2)

testPowerOfTwo1 = TestCase (assertEqual "powerOfTwo' 0"  1  (powerOfTwo' 0))
testPowerOfTwo2 = TestCase (assertEqual "powerOfTwo' 1"  2  (powerOfTwo' 1))
testPowerOfTwo3 = TestCase (assertEqual "powerOfTwo' 2"  4  (powerOfTwo' 2))
testPowerOfTwo4 = TestCase (assertEqual "powerOfTwo' 3"  8  (powerOfTwo' 3))
testPowerOfTwo5 = TestCase (assertEqual "powerOfTwo' 4" 16  (powerOfTwo' 4))

testsPowerOfTwo = TestList [testPowerOfTwo1, testPowerOfTwo2, testPowerOfTwo3, testPowerOfTwo4, testPowerOfTwo5]

-- don't run.
--prop_powerOfTwo n =
--   powerOfTwo n  ==  2^n

-- Ex 4.49 Write QuickCheck test for the exercises:  See above.
