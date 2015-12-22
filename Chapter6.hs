-- | Chapter 6 - Programming with Lists

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


-- Ex 6.2 -- [[a]] -> [[a]] - isn't the most general type for 'id', it is
-- constrained by a list of list of some type a.  A type 'a'
-- hold more types including lists of lists


-- Ex 6.3 - what is the most general type of shift.

--shift :: ((Integer,Integer),Integer) -> (Integer,(Integer,Integer))
shift :: ((a,b),c) -> (a,(b,c))
shift ((x,y),z) = (x,(y,z))


-- 6.2 - Haskell list funtions in the Prelude


-- 6.3 Finding your way around the Haskel libraries


-- The Picture example: Implementation

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

rotate :: Picture -> Picture
rotate p = flipH (flipV p)


invertChar :: Char -> Char
invertChar ch  =   if ch=='.' then '#' else '.'

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
-- ok.

-- Ex 6.12
prop_besideFlipV :: Picture -> Picture -> Bool
prop_besideFlipV  p1 p2 =
    flipV (p1 `beside` p2) == flipV p2 `beside` flipV p1

prop_besideFlipH :: Picture -> Picture -> Property
prop_besideFlipH p1 p2 =
    length p1 == length p2
    ==>
    flipH (p1 `beside` p2) == flipH p1 `beside` flipH p2

-- What! -> *** Gave up! Passed only 44 tests.


-- Ex 6.13 - four pictures using above and beside
prop_aboveBeside :: Picture -> Bool
prop_aboveBeside pic =
    (pic `above` pic) `beside` (pic `above` pic)
    ==
    (pic `beside` pic) `above` (pic `beside` pic)


-- Ex 6.14 -- property of rotate90

prop_rotate90 :: Picture -> Property
prop_rotate90 p =
    rectangular p && notElem "" p
    ==>
    p == rotate90 (rotate90 (rotate90 (rotate90 p)))


-- Ex 6.15

prop_invertColor :: Picture -> Bool
prop_invertColor p1 =
    p1 == invertColour (invertColour p1)
-- no, it doesn't hold for randomly generated data


-- Ex 6.16 analogue ot prop_aboveBeside3Correct

prop_besideAbove3Correct :: Picture -> Picture -> Bool
prop_besideAbove3Correct n s =
    (n `beside` n) `above` (s `beside` s)
    ==
    (n `above` s) `beside` (n `above` s)

-- No, don't seem to need the filter. hmmm.



-- 6.5 Extended exercise: alternative implementations

-- Ex 6.17

-- Ex 6.18


-- Ex 6.19  Binary pictures

type PictureB = [[Bool]]   -- almost a bit map

black :: Bool
black = True

white :: Bool
white = False


flipHB :: PictureB -> PictureB
flipHB = reverse

aboveB :: PictureB -> PictureB -> PictureB
aboveB = (++)

flipVB :: PictureB -> PictureB
flipVB pic = [reverse line | line <- pic]


besideB :: PictureB -> PictureB -> PictureB
besideB pL pR =
    [lineL ++ lineR | (lineL,lineR) <- zip pL pR]


rotateB :: PictureB -> PictureB
rotateB p =  flipHB (flipVB p)


invertCharB :: Bool -> Bool
invertCharB   = not     -- simple, well nigh unnessary

invertLineB :: [Bool] -> [Bool]
invertLineB line =
    [invertCharB ch | ch <- line]

invertColourB :: PictureB -> PictureB
invertColourB pic =
    [invertLineB line | line <- pic]



printPictureB :: PictureB -> IO ()
printPictureB p = putStr (concat [])


-- Ex 6.20  - picture a list of columns

type PictureCol = [[Char]]



-- Supermarket billing

type Name    = String
type Price   = Int
type BarCode = Int

type Database = [(BarCode,Name,Price)]

codeIndex :: Database
codeIndex = [ (4719, "Fish Fingers", 121),
              (5643, "Nappies", 1010),
              (3814, "Orange Jelly", 56),
              (1111, "Hula Hoops", 21),
              (1112, "Hula Hoops (Giant)", 133),
              (1234, "Dry Sherry, 1lt", 540)]


type TillType = [BarCode]

type BillType = [(Name, Price)]

produceBill :: TillType -> String
produceBill = formatBill . makeBill

lineLength :: Int
lineLength = 30

-- Ex. 6.39

formatPence :: Price -> String
formatPence pr =  pounds ++ "." ++ pence
  where pounds = show (pr `div` 100)
        pnc    = pr `mod` 100
        pence = if pnc > 9
                then show pnc
                else "0" ++ show pnc
-- Ex 6.40

format :: String -> String -> String
format str1 str2 = str1 ++ buff ++ str2
  where buff = ['.' | _ <- [1..n]]
        n = lineLength - length str1 - length str2

formatLine  :: (Name,Price) -> String
formatLine (nm,pr) = format nm pence ++ "\n"
  where pence = formatPence pr

-- Ex 6.41

formatLines :: [(Name,Price)] -> String
formatLines lst = concat [formatLine ln | ln <- lst]

-- Ex 5.42

makeTotal :: BillType -> Price
makeTotal bt = sum [pr | (_,pr) <- bt]

-- Ex 6.43

formatTotal :: Price -> String
formatTotal pr = "\n" ++ format "Total" pence
  where pence = formatPence pr

-- Ex 6.44

formatHeader :: String -> String
formatHeader name = replicate n ' ' ++ name ++ "\n"
  where n = (lineLength - length name) `div` 2

formatBill' :: BillType -> String
formatBill' bt = formatHeader "Haskell Stores" ++ "\n" ++
                 formatLines bt ++ "\n" ++
                 formatTotal (makeTotal bt)

bill1 :: BillType
bill1 = [("Dry Sherry, 1lt",540),("Fish Fingers",121),
         ("Orange Jelly", 56), ("Hula Hoops (Giant)",133),
         ("Unknown Item",0),("Dry Sherry, 1lt", 540)]

-- Ex 6.45

look' :: Database -> BarCode -> [(Name,Price)]
look' db bc =  [(nm,pr) | (bc',nm,pr) <- db, bc == bc']

look :: Database -> BarCode -> (Name,Price)
look db bc = if not (null rtn)
             then head rtn
             else ("Unknown Item",0)
  where rtn = look' db bc

-- Ex 6.46
lookup' :: BarCode -> (Name,Price)
lookup' = look codeIndex


-- Ex 6.47
makeBill   :: TillType -> BillType
makeBill till = [lookup' bc | bc <- till]


til :: TillType
til = [1234,4719,3814,1113,1234]


-- Extending the problem

-- Ex 6.48

makeDiscount :: BillType -> Price
makeDiscount bt = (countSherry bt `div` 2) * 100

countSherry :: BillType -> Int
countSherry []  = 0
countSherry (x:xs)
  | fst x == "Dry Sherry, 1lt" = 1 + countSherry xs
  | otherwise                  = countSherry xs

formatDiscount :: Price -> String
formatDiscount pr = format "Discount" (formatPence pr) ++ "\n"

formatBill :: BillType -> String
formatBill bt = formatHeader "Haskell Stores" ++ "\n" ++
                formatLines bt ++ "\n" ++
                formatDiscount discount ++ 
                formatTotal total
  where discount = makeDiscount bt
        total    = makeTotal bt  - discount

-- 6.49  database functions

type Record = (BarCode,Name,Price)

update :: Database -> Record -> Database
update db (bc,nm,pr) = add db2 (bc,nm,pr)
  where db2 = remove db bc

remove :: Database -> BarCode -> Database
remove db bc
  = [(bc',name,price) | (bc',name,price) <- db, bc' /= bc]

add :: Database -> Record -> Database
add db rc = rc : db
            
