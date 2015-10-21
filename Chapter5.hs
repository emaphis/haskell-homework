-- | Data types, tuples, and lists

module Chapter5 where
import Test.HUnit
import Test.QuickCheck
import Data.Char

-- tuple - combination of a fixed number of things of various types
-- list - a variable number of things of the same type

-- individual items:

-- ShopItem :: (String, Int)
-- ("Salt: 1kg", 139)
-- ("Plain crips", 25)

type ShopItem = (String, Int)

salt = ("Salt: 1kg", 139) :: ShopItem
crisps = ("Plain crips", 25) :: ShopItem

-- The shopping basket

type Basket = [ShopItem]

basket :: Basket
basket = [ ("Salt: 1kg",139) , ("Plain crisps",25) , ("Gin: lit",1099) ]

basket2 = [salt, crisps] :: Basket


-- 5.2 Tuple types

-- 1 we can return compound types:

minAndMax :: Integer -> Integer -> (Integer, Integer)
minAndMax x y
  | x>=y       = (y,x)
  | otherwise  = (x,y)

 -- Signal if a solution has been foune:  (Any, Bool)


-- Pattern matiching:
-- funtions over tuples are usually defined using pattern matching

addPair :: (Integer, Integer) -> Integer
addPair (0,y) = y
addPair (x,0) = x
addPair (x,y) = x+y


name :: ShopItem -> String
name (n,p) = n

price :: ShopItem -> Int
price (n,p) = p

-- selector functions
name' :: ShopItem -> String
name' si = fst si

price' :: ShopItem -> Int
price' si = snd si


addPair' :: (Integer,Integer) -> Integer
addPair' p = fst p + snd p


-- fibonacci defined using tuple
fibStep :: (Integer,Integer) -> (Integer,Integer)
fibStep (u,v) = (v,u+v)

fibPair :: Integer -> (Integer,Integer)
fibPair n
  | n==0       = (0,1)
  | otherwise  = fibStep (fibPair (n-1))


fastFib :: Integer -> Integer
fastFib = fst . fibPair


-- Exercises pg 103

-- Ex 5.1  define maxOccurs

maxOccurs :: Integer -> Integer -> (Integer,Integer)
maxOccurs x y
  | x==y       = (x, 2)
  | otherwise  = (max x y, 1)

test_maxOccurs = TestList
   [ TestCase (assertEqual "none equal"     (2,1) (maxOccurs 1 2)),
     TestCase (assertEqual "rev none equal" (2,1) (maxOccurs 2 1)),
     TestCase (assertEqual "two equal"      (2,2) (maxOccurs 2 2))
   ]

maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer,Integer)
maxThreeOccurs x y z = (max (fst m) z, n)
  where m = maxOccurs x y
        n = if z == fst m
            then 1 + snd m
            else snd m
-- 1 2 3, 3 2 1, 2 3 1, 2 3 3, 3 3 3

test_maxThreeOccurs = TestList [
     TestCase (assertEqual "none equal"     (3,1) (maxThreeOccurs 1 2 3)),
     TestCase (assertEqual "rev none equal" (3,1) (maxThreeOccurs 3 2 1)),
     TestCase (assertEqual "mix none equal" (3,1) (maxThreeOccurs 2 3 1)),
     TestCase (assertEqual "two equal"      (3,2) (maxThreeOccurs 1 3 3)),
     TestCase (assertEqual "all equal"      (3,3) (maxThreeOccurs 3 3 3))
   ]

-- Ex 5.2 - orderTriple -
-- returns a triple in assending order given a triple

orderTriple :: (Integer,Integer,Integer) -> (Integer,Integer,Integer)
orderTriple (m, n, o) = (mn, md, mx)
  where
    mn = min m (min n o)
    mx = max m (max n o)
    md = (m+n+o) - (mn + mx)   -- cheat found on line

test_orderTriple = TestList [
     TestCase (assertEqual "in order"      (1,2,3) (orderTriple (1,2,3))),
     TestCase (assertEqual "reverse order" (1,2,3) (orderTriple (3,2,1))),
     TestCase (assertEqual "mixed order"   (1,2,3) (orderTriple (2,3,1)))
   ]


-- Ex 5.3 - intercept x axis


xAxis :: Float -> Float -> (Float, Bool)
xAxis y c
  | y==0.0     = (0.0, False)   -- 0 slope so now inersept
  | otherwise  = (y/c, True)



-- 5.3 Introducing algerbraic types


-- Enumerated types:

data Move = Rock | Paper | Scissors
          deriving (Eq, Show)


-- score - pattern over an enumerated tpe
score :: Move -> Move -> Integer
score Rock  Rock      = 0
score Rock  Paper     = -1
score Rock  Scissors  = 1
score Paper Rock      = 1


-- Product types:

data People = Person Name Age
              deriving (Eq, Show)

type Name = String
type Age  = Int


jemima = Person "Electric Aunt Jemima" 77
ronnie = Person "Ronnie" 14

-- pattern matching on a product type
showPerson :: People -> String
showPerson (Person st n) = st ++ " -- " ++ show n


-- Tuples and data types

type People' = (Name,Age)


-- Alternatives:

data Shape = Circle Float |
             Rectangle Float Float |
             Triangle Float Float Float
             deriving (Ord, Show)

circle1 = Circle 3.0
rectangle1 = Rectangle 45.9 87.6
triangle1 = Triangle 4.0 5.0 9.0

-- pattern matching on alternatives
isRound :: Shape -> Bool
isRound (Circle _)        = True
isRound (Rectangle _ _)   = False
isRound Triangle{}  = False

area :: Shape -> Float
area (Circle r)        = pi*r*r
area (Rectangle h w)   = h*w
area (Triangle a b c)  = a + b + c


-- Exercises

-- Ex 5.5 define a perimeter function on shape

perimeter :: Shape -> Float
perimeter (Circle r)       = 2.0 * pi * r
perimeter (Rectangle w h)  = 2.0 * (w + h)
perimeter (Triangle a b c) =
  sqrt (sp * (sp-a) * (sp-b) * (sp-c))  -- heron's formula
    where sp = (a+b+c) / 2.0


-- Ex 5.6 - redefine Item definition using a data definition

data Item =  Shopltem Name Price

-- type Name  = String
type Price = Integer


-- Ex 5.7 - add trianle to definition of Shape above


-- Ex 5.8  - find regular shapes, equal sides

isRegular :: Shape -> Bool
isRegular (Circle _)       = True
isRegular (Rectangle w h)  = w == h
isRegular (Triangle a b c) =  a == b && a == c


-- Ex 5.9 - investigate Move and Shape what form does 'show' take

-- show :: a -> String

-- show (Circle 3.0) => "Circle 3.0"
-- show (Rectangle 2.0 3.0) => "Rectangle 2.0 3.0"
-- show Rock => "Rock"
-- show Paper => "Paper"

-- Circle 3.0  == Circle 3.0 => True

-- Ex 5.10 - define (==) over shape

instance Eq Shape
    where
      (Circle r1) == (Circle r2)  =
          abs r1 == abs r2
      (Rectangle w1 h1) == (Rectangle w2 h2) =
          abs w1  == abs w2 &&  abs h1  ==  abs h2
      (Triangle a1 b1 c1) == (Triangle a2 b2 c2) =
          abs a1 == abs a2 &&
          abs b1 == abs b2 &&
          abs c1 == abs c2
      _ == _ = False


-- Ex 5.11  add location to Shape

data Point = Point Float Float
           deriving (Eq,Ord,Show)

data NewShape = Circle' Float Point |
                Rectangle' Float Float Point |
                Triangle' Float Float Float Point
                deriving (Eq,Ord,Show)

-- Ex 5.12 - define a move funtion for new shape
move :: Float -> Float -> NewShape -> NewShape
move x y (Circle' r (Point x1 y1))  =
    Circle' r (Point (x1+x) (y1+y))
move x y (Rectangle' h w (Point x1 y1)) =
    Rectangle' h w (Point (x1+x) (y1+y))
move x y (Triangle' a b c (Point x1 y1)) =
     Triangle' a b c (Point (x1+x) (y1+y))


-- Ex 5.13  TODO: don't know how, yet.


-- Ex 5.14

data House = Name String |
             Number Integer
             deriving (Show)

toText :: House -> String
toText (Name h)   = h
toText (Number a) = show a

data Address = Address House String String
             deriving (Show)

someHouse = Number 7923
someAddress = Address someHouse "Buster Dr" "Lamesville"



--  5.5 Lists in Haskell

list1 = [1,2,3,4,1,4] :: [Integer]
list2 = [True]  :: [Bool]

list3 = ['a', 'a', 'b']  :: String
list4 = "aab" :: String

list5 = [fastFib, fastFib] :: [Integer -> Integer]
list6 = [[12,2], [2,12],[]]  :: [[Integer]]


-- The String type

-- type String = [Char]

-- Exercises

-- Ex 5.15  what is the value of:

list7 = [0, 0.1 .. 1]

-- [0.0,0.1,0.2,0.30000000000000004,0.4000000000000001,0.5000000000000001,0.6000000000000001,0.7000000000000001,0.8,0.9,1.0]


-- Ex 5.16  how many items to the following lists contain

list8  = [2,3]   -- 2
list9  = [[2,3]]  -- 1 -- type:  [[Integer]]


-- Ex 5.17 evaluating bad lists

-- [2 .. 2] -- 2
-- [2,7 .. 4] -- 2


-- List comprehensions


-- Examples

-- 1.
ex = [2,4,7]
list10 = [2*n | n<-ex] -- 'Take all 2*n where n comes from ex.'
list10a = [2*n | n<-[2,4,7]]

-- 2.

isEven :: Integer -> Bool
isEven m = m `mod` 2 == 0

list11 = [isEven n | n<-ex] -- [True,True,False]

-- 3. a generator may have more than one test

-- 'Take all 2*n where n comes from ex, n is even and greater than 3.'
list12 = [2*n | n <- ex, isEven n, n>3]


-- 4. patterns:

addPairs :: [(Integer,Integer)] -> [Integer]
addPairs pairList = [m+n | (m,n) <- pairList]

ex2 = [(2,3),(2,1),(7,8)]

list13 = addPairs ex2   -- [5,3,15]


-- 5. add tests

addOrdPairs :: [(Integer,Integer)] -> [Integer]
addOrdPairs pairList = [m+n | (m,n) <- pairList, m<n]

list14 = addOrdPairs ex2  -- [5,15]


-- 6. filtering digits

digits :: String -> String
digits st = [ch | ch<-st, isDigit ch]


-- 7.  list comprehensions can form part of a larget definition

allEven, allOdd :: [Integer] -> Bool
allEven xs = xs == [x | x<-xs, isEven x]
allOdd  xs = [] == [x | x<-xs, isEven x]


-- 8.

totalRadii :: [Shape] -> Float
totalRadii shapes = sum [r | Circle r <- shapes]

flt1 = totalRadii [Circle 2.1, Rectangle 2.1 3.2, Circle 4.7]
-- => 6.8

-- extract all of the singleton items froma list

sings :: [[Integer]] -> [Integer]
sings xss = [x | [x] <- xss]  -- match on singletons

list15 = sings [[],[1],[2,3],[4],[5,6,7],[8]]
--  [1,4,8]


-- Exercises

-- Ex 5.18. - double all in a list

doubleAll :: [Integer] -> [Integer]
doubleAll xs = [2*x | x <-xs]


-- Ex. 5.19 - capitalize String

capitalize :: String -> String
capitalize str = [toUpper c | c<-str]


-- remove all not letters from the string
capitalizeLetters :: String -> String
capitalizeLetters str = [toUpper c | c<-str, isAlpha c]


-- Ex 5.20  - divisors of a number

divisors :: Integer -> [Integer]
divisors n = [m | m<-[2 .. n-1], (n `mod` m) == 0]

isPrime :: Integer -> Bool
isPrime n = divisors n == []


-- Ex 5.21

matches :: Integer -> [Integer] -> [Integer]
matches n xs = [x | x<-xs, x==n]

elem' :: Integer -> [Integer] -> Bool
elem' n xs = length (matches n xs) > 0


-- Ex 5.22

onSeparateLines :: [String] -> String
onSeparateLines xs = concat [str ++ "\n" | str<-xs]


-- Ex 5.23 copy a string 'n' times

duplicate :: String -> Integer -> String
duplicate str n = [st |  i <- [1..n], st<-str]


-- Ex 5.24  -- right justify a string

linelength = 12

pushRight :: String -> String
pushRight str = [' ' | i<- [1 .. linelength - (length str)]] ++ str


-- Ex 5.25  pushRight doesn't handle when 'str' is longer than 'linelength'


-- Ex 5.26

fibTable :: Integer -> String
fibTable n = onSeparateLines (["n" ++ pushRight "fib n"] ++
                             [show m ++ pushRight (show (fastFib i)) | i<-[1..n]])

-- putStr (fibTable 10)



-- 5.7 - A library database

