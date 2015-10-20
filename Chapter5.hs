-- | Data types, tuples, and lists

module Chapter5 where
import Test.HUnit
import Test.QuickCheck

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
