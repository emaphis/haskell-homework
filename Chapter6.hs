-- | Programming with Lists

module Chapter6 where

--import Test.HUnit
import Prelude hiding (id)
import Test.QuickCheck

-- 6.1  Generic functions: polymorphism

id :: a -> a
id x = x

mystery :: (Bool, a) -> Char
mystery (x,_) = if x then 'c' else 'd'


-- Polymorphism and overloading

-- polymorphic - same definition over different types
-- overloaded  - different definition over different types


-- Exercises

-- Ex 6.1 - give the most general type for:

snd' :: (a, b) -> b
snd' (_, y) = y

sing :: a -> [a]
sing x = [x]


-- Ex 6.2 -- [[a]] -> [[a]] - isn't the most general type, it is
-- constrained ot by a list of list of some type a.  A type 'a'
-- hold more types including lists of lists


-- Ex 6.3 - what is the most general type of shift.

--shift :: ((Integer,Integer),Integer) -> (Integer,(Integer,Integer))
shift :: ((a,b),c) -> (a,(b,c))
shift ((x,y),z) = (x,(y,z))


-- 6.2 - Haskell list funtions in the Prelude


-- 6.3 Finding your way around the Haskel libraries


-- The Pircture example: Implementation

type Picture = [[Char]]


flipH  :: Picture -> Picture
flipH = reverse

above :: Picture -> Picture -> Picture
above = (++)

flipV  :: Picture -> Picture
flipV pic = [reverse line | line <- pic]

beside :: Picture -> Picture -> Picture
beside picL picR
    = [lineL ++ lineR | (lineL,lineR) <- zip picL picR]

invertChar :: Char -> Char
invertChar ch = if ch=='.' then '#' else '.'

invertLine :: [Char] -> [Char]
invertLine line
    = [invertChar ch | ch <- line]

invertColour :: Picture -> Picture
invertColour pic = [invertLine line | line <- pic]


-- Tests and properties

prop_AboveFlipV :: Picture -> Picture -> Bool
prop_AboveFlipV pic1 pic2 =
    flipV (pic1 `above` pic2)  ==  flipV pic1 `above` flipV pic2


prop_AboveFlipH :: Picture -> Picture -> Bool
prop_AboveFlipH pic1 pic2 =
    flipH (pic1 `above` pic2)  ==  flipH pic2 `above` flipH pic1


prop_AboveBeside :: Picture -> Picture -> Picture -> Picture -> Bool
prop_AboveBeside nw ne sw se =
    (nw `beside` ne) `above` (sw `beside` se)
    ==
    (nw `above` sw) `beside` (ne `above` se)

prop_AboveBeside3Correct :: Picture -> Picture -> Property
prop_AboveBeside3Correct w e =
    (rectangular w && rectangular e && height w == height e)
    ==>
       (w `beside` e) `above` (w `beside` e)
         ==
       (w `above` w) `beside` (e `above` e)


notEmpty :: Picture -> Bool
notEmpty p = p /= []

rectangular :: Picture -> Bool
rectangular p =
    notEmpty p &&
    and [length first == length ln | ln <- rest]
        where (first:rest) = p

height, width :: Picture -> Int
height p = length p

width p = length (head p)


size :: Picture -> (Int,Int)
size p = (width p, height p)

-- Exercises:

-- Ex 6.4  define superimposeChar

superimposeChar :: Char -> Char -> Char
superimposeChar c1 c2
    | c1 == '.' && c2 == '.'  = '.'
    | otherwise  = '#'


-- simpler - pattern matching:
superimposeChar' :: Char -> Char -> Char
superimposeChar' '.' '.' = '.'
superimposeChar'  _   _  = '#'


-- Ex 6.5

superimposeLine :: [Char] -> [Char] -> [Char]
superimposeLine l1 l2 =
    [superimposeChar c1 c2 | (c1,c2) <- zip l1 l2]


-- Ex 6.6

superimpose :: Picture -> Picture -> Picture
superimpose p1 p2 =
    [superimposeLine l1 l2 | (l1,l2) <- zip p1 p2]


-- Ex 6.7
printPicture :: Picture -> IO ()
printPicture p = putStr (concat [l1 ++ "\n" | l1 <- p])

-- or
-- printpicture p = putStr (onSeparatelines p)

pic1 :: Picture
pic1 = [".##.", ".#.#", ".###", "####"]


-- Ex 6.8

rotate90 :: Picture -> Picture
rotate90 p1 = [slice n p1 | n <- [0 .. num]]  -- count the lines
    where
      num = length p1 - 1             -- num of lines in pic
      slice n pic = reverse [l1 !! n | l1 <- pic] -- and dice

rotate90' :: Picture -> Picture
rotate90' p1 = [[ln !! n | ln <- reverse p1] | n <- [0..num]]
    where num = length p1 - 1

-- Ex 6.9
revrotate90 :: Picture -> Picture
revrotate90 p1 = rotate90 (rotate90 (rotate90 p1))


-- Ex 6.10 - scaling

scale :: Picture -> Int -> Picture
scale p1 n
    | n <= 0 || p1 == [[]]  = [[]]
    | otherwise  = concat [replicate n (concat [replicate n ch1 | ch1 <- ln1 ]) | ln1 <- p1]


-- Ex 6.11 - correct the property prop_aboveFlipH - above

-- Ex 6.12


-- Ex 6.13 - four pictures using above and beside
prop_aboveBeside :: Picture -> Bool
prop_aboveBeside pic =
    (pic `above` pic) `beside` (pic `above` pic) ==
    (pic `beside` pic) `above` (pic `beside` pic)

-- Ex 6.14 -- property of rotate90

prop_rotate90 :: Picture -> Bool
prop_rotate90 p =
    p == rotate90 (rotate90 (rotate90 (rotate90 p)))


-- Ex 6.15

prop_invertColor :: Picture -> Bool
prop_invertColor p1 =
    p1 == invertColour (invertColour p1)
-- no, it doesn't hold for randomly generated data

