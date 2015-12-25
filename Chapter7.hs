-- | Chapter 7 - Defining functions over lists

module Chapter7 where

import Chapter5 (digits,isEven)
import Test.QuickCheck
import Test.HUnit

-- 7.1 Pattern matching revisted

mystery,mystery' :: Integer -> Integer -> Integer
mystery x y
  | x==0      = y
  | otherwise = x

mystery' 0 y = y
mystery' x _ = x

-- Patterns can match:
-- literal value
-- variable
-- wildcard '_'
-- constructor


-- 7.2 Lists and list patterns

-- a list is either empty [] of non-empty

-- cons -> [1,2,3,4] -> 1:2:3:4:[]

-- x:y:zs  -> x:(y:zs)

-- Pattern matching definitions

-- fun xs = ...   will match all lists

-- distinguish differnt states of lists:

head'       :: [a] -> a
head' (x:_)  = x

tail'       :: [a] -> [a]
tail' (_:xs) = xs

null'       :: [a] -> Bool
null' []    = True
null' (_:_) = False

-- The 'case' construction

firstDigit  :: String -> Char
firstDigit st
  = case (digits st) of
     []     -> '\0'
     (x:_)  -> x


-- Exercises

-- Ex 7.1
plusone      :: [Int] -> Int
plusone []    = 0
plusone (x:_) = x+1


-- Ex 7.2
add2        :: [Int] -> Int
add2 []      = 0
add2 [x]     = x
add2 (x:y:_) = x+y


-- Ex 7.3

plusone'     :: [Int] -> Int
plusone' xs
  | null xs   = 0
  | otherwise = head xs + 1

prop_plussone ns  = plusone ns == plusone' ns


add2'        :: [Int] -> Int
add2' xs
  | len == 0  = 0
  | len == 1  = head xs
  | otherwise = head xs +  head (tail xs)
  where len = length xs

prop_add2 ns = add2 ns == add2' ns

-- Ex 7.4

firstDigit'  :: String -> Char
firstDigit' xs
  | null ys   = '\0'
  | otherwise = head ys
  where ys = digits xs

prop_firstDigit str  = firstDigit str == firstDigit' str


-- 7.3 Primitive recursion over lists

sum'       :: [Integer] -> Integer
sum' []     = 0
sum' (x:xs) = x + sum' xs

prop_sum xs = sum' xs == sum xs


-- Exercises

-- Ex 7.5

product'       :: [Integer] -> Integer
product' []     = 1
product' (x:xs) = x * product' xs

prop_product xs = product' xs == product xs


-- Ex 7.6

and', or'   :: [Bool] -> Bool

and' []     = True
and' (b:bs) = b && and' bs

or' []      = False
or' (b:bs)  = b || or' bs

prop_and bs = and' bs  == and bs
prop_or bs  = or' bs  == or bs



-- 7.4  Finding primitive recursive definitons

-- template for primitive recursion
-- fun []     = ...
-- fun (x:xs) = ... x ... xs ... fun xs ...

concat'       :: [[a]] -> [a]
concat' []     = []
concat' (x:xs) =  x ++ concat' xs

prop_concat xs = concat' xs == concat xs


-- (++)
-- [2,3,4] ++ [9,8]   => [2,3,4,9,8]
--   [3,4] ++ [9,8]   => [3,4,9,8]
--     [4] ++ [9,8]   =? [9,6]

(+++)  :: [a] -> [a] -> [a]
[]     +++ ys = ys
(x:xs) +++ ys = x : (xs+++ys)

prop_append xs ys  = (xs+++ys)  == (xs++ys)


elem'         :: Integer -> [Integer] -> Bool
elem' _ []     = False
elem' n (x:xs) = (n==x) || elem' n xs
--elem' n (x:xs)
--  | n==x      = True
--  | otherwise = elem' n xs

prop_elem n xs = elem' n xs == elem n xs


-- Repeated variables in patterns - do not compile

--elem2   :: Eq a => a -> [a] -> Bool
--elem2 x (x:ys) = True
--elem2 x (y:ys) = elem2 x ys


-- double every element in an integer list - map

doubleAll1,doubleAll2  :: [Integer] -> [Integer]
doubleAll1 []     = []
doubleAll1 (n:ns) = (2*n) : doubleAll1 ns

doubleAll2 ns = [2*n | n<-ns]

prop_doubleAll xs = doubleAll1 xs == doubleAll2 xs


-- select all of the even numbers in a list - filter

selectEven1,selectEven2 :: [Integer] -> [Integer]

selectEven1 ns = [n | n<-ns, isEven n]

selectEven2 []   = []
selectEven2 (n:ns)
  | isEven n     = n : selectEven2 ns
  | otherwise    = selectEven2 ns

prop_selectEven xs =  selectEven1 xs == selectEven2 xs



-- sorting a list of integers using and insert functions

ins :: Integer -> [Integer] -> [Integer]
ins n []    = [n]
ins n (x:xs)
  | n <= x     = n : x : xs
  | otherwise  = x : ins n xs


iSort :: [Integer] -> [Integer]
iSort []     = []
iSort (n:ns) = ins n (iSort ns)


-- Exercises

-- Ex 7.7  properties for functions - see above


-- Ex 7.8 - number of given element in a list
elemNum1,elemNum2  :: Integer -> [Integer] -> Integer

elemNum1 _ []  = 0
elemNum1 n (x:xs)
  | n==x       = 1 + elemNum1 n xs
  | otherwise  = elemNum1 n xs


elemNum2 n xs = sum [1 | x<-xs, x==n]

prop_elemNum n ns = elemNum1 n ns == elemNum2 n ns


-- Ex 7.9 - list of numbers that occure once in a list

unique1,unique2 :: [Integer] -> [Integer]

unique1 []   = []
unique1 (n:ns)
  | elemNum1 n ns == 0  = n : unique1 ns
  | otherwise           = unique1 ns

unique2 ns = [n | n<-ns, elemNum1 n ns == 1]

prop_unique ns  = unique1 ns  == unique2 ns


-- Ex 7.10 -- property combining unique and elemNum
-- TODO:  ???


-- Ex 7.11 - reverse, unzip

reverse'       :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

prop_reverse1 xs = reverse' xs == reverse xs

prop_reverse2 xs = reverse' (reverse' xs) == xs

unzip'        :: [(a,b)] -> ([a],[b])
unzip' []     = ([],[])
unzip' ((x,y):zs) = (x:xs, y:ys)
  where (xs,ys) = unzip' zs

prop_unzip xs = unzip' xs == unzip xs


-- 7.12 - max min

maximum'  :: [Integer] -> Integer
maximum' xs = last (iSort xs)

minimum'   :: [Integer] -> Integer
minimum' xs = head (iSort xs)

prop_max xs = xs /= []
              ==> maximum' xs == maximum xs

prop_min xs = xs /= []
              ==> minimum' xs == minimum xs

maximum2  :: [Integer] -> Integer
maximum2 [x] = x
maximum2 (x:xs) = x `max` maximum2 xs

minimum2  :: [Integer] -> Integer
minimum2 [x] = x
minimum2 (x:xs) = x `min` minimum2 xs

prop_max2 xs = xs /= []
               ==> maximum2 xs == maximum xs

prop_min2 xs = xs /= []
               ==> minimum2 xs == minimum xs


-- Ex 7.13 -- test data for 'ins'
tstIns1 = TestCase (assertEqual "ins empyt"    [1]       (ins 1 []))
tstIns2 = TestCase (assertEqual "ins first"    [1,2,3]   (ins 1 [2,3]))
tstIns3 = TestCase (assertEqual "ins last"     [1,2,3]   (ins 3 [1,2]))
tstIns4 = TestCase (assertEqual "ins middle"   [1,2,3]   (ins 2 [1,3]))
tstIns5 = TestCase (assertEqual "ins existing" [1,2,2,3] (ins 2 [1,2,3]))

testIns = TestList [tstIns1,tstIns2,tstIns3,tstIns4,tstIns5]


-- Ex 7.14

isSorted :: [Integer] -> Bool
isSorted []  = True
isSorted [_] = True
isSorted (x:y:zs)
  | x > y   = False
  | otherwise = isSorted (y:zs)

-- Examples
tstSrt1 = TestCase (assertEqual "is srt base"  True  (isSorted []))
tstSrt2 = TestCase (assertEqual "is srt one"   True  (isSorted [1]))
tstSrt3 = TestCase (assertEqual "is srt 2"     True  (isSorted [1,2]))
tstSrt7 = TestCase (assertEqual "is srt 3"     True  (isSorted [1,2,3]))
tstSrt4 = TestCase (assertEqual "is srt 3 not" False (isSorted [2,1]))
tstSrt5 = TestCase (assertEqual "is srt 4 not" False (isSorted [1,2,3,2]))
tstSrt6 = TestCase (assertEqual "is srt 4 dbl" True  (isSorted [1,2,2,3]))

testIsSrt = TestList [tstSrt1,tstSrt2,tstSrt3,tstSrt4,tstSrt5,tstSrt6,tstSrt7]
-- ins and iSort properties
prop_iSort xs = isSorted (iSort xs)
prop_ins n xs = isSorted (ins n (iSort xs))


-- Ex 7.15 - sorting functions shouldn't delete elements
          -- This shoule already be tested in regular test
          -- Sorted list should equal re-sorted lists
prop_iSort2 xs = iSort sorted == sorted
                 where sorted = iSort xs


-- Ex 7.16 - descending sort, remove duplicate sort

-- Sort in descending order
dIns :: Integer -> [Integer] -> [Integer]
dIns n []    = [n]
dIns n (x:xs)
  | n >= x     = n : x : xs
  | otherwise  = x : dIns n xs

dSort :: [Integer] -> [Integer]
dSort []     = []
dSort (n:ns) = dIns n (dSort ns)


-- Sort removing duplicates

rIns :: Integer -> [Integer] -> [Integer]
rIns n []    = [n]
rIns n (x:xs)
  | n < x      = n : x : xs
  | n == x     = rIns n xs
  | otherwise  = x : rIns n xs

rSort :: [Integer] -> [Integer]
rSort []     = []
rSort (n:ns) = rIns n (rSort ns)


-- Ex 7.17 -
--  yes iSorted will have to change for descending sort
--  isSorted will still work for deplicates removed


-- Ex 7.18  - test data for remove duplicates

testRSort = TestList [
  TestCase (assertEqual "rSort base case"  []       (rSort [])),
  TestCase (assertEqual "rSort test 2"     [1,2]    (rSort [1,2])),
  TestCase (assertEqual "rSort test 3"     [1,2,3]  (rSort [3,1,2])),
  TestCase (assertEqual "rSort test dup"   [1,2,3]  (rSort [3,2,1,2])),
  TestCase (assertEqual "rSort test dup"   [2]      (rSort [2,2,2]))

 ]

-- Ex 7.19  - sorting pairs

pSort :: [(Integer,a)] -> [(Integer, a)]
pSort []  = []
pSort ((n,a):xs) = pIns (n,a) (pSort xs)

pIns :: (Integer,a) -> [(Integer,a)] -> [(Integer,a)]
pIns n []     = [n]
pIns (n,a) ((m,b):xs)
  | n <= m    = (n,a) : (m,b) : xs
  | otherwise = (m,b) : pIns (n,a) xs


-- 7.5  General recursions over lists

-- recursion over two inputs

zip' :: [a] -> [b] -> [(a,b)]
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
zip' _ _ = []

prop_zip xs ys = zip' xs ys  ==  zip xs ys

take' :: Int -> [a] -> [a]
take' 0  _  = []
take' _ []  = []
take' n (x:xs) = x : take' (n-1) xs

prop_take n xs = n >= 0
                 ==> take' n xs == take n xs


-- Exercises

-- Ex 7.20 - splitAt

drop' :: Int -> [a] -> [a]
drop' _ []     = []
drop' 0 xs     = xs
drop' n (_:xs) = drop' (n-1) xs

prop_drop n xs = n >= 0
                 ==>  drop' n xs  ==  drop n xs

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' _ []  = ([],[])
splitAt' 0 xs  = ([], xs)
splitAt' n xs  = (take n xs, drop n xs)

prop_splitAt n xs = splitAt' n xs == splitAt n xs


-- Ex 7.22

zip1 :: ([a],[b]) -> [(a,b)]
zip1 (xs,ys) = zip xs ys

-- unzip :: [(a,b)] -> ([a],[b])

prop_uzip_zip :: (Eq b, Eq a) => ([a], [b]) -> Bool
prop_uzip_zip xs = unzip(zip1 xs) == xs
-- *** Failed! Falsifiable (after 2 tests):
-- ([()],[])

prop_zip_uzip :: (Eq b, Eq a) => [(a, b)] -> Bool
prop_zip_uzip xs = zip1(unzip xs) == xs


-- Ex 7.23

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3' xs ys zs
zip3' _ _ _ = []

prop_zip3 xs ys zs = zip3' xs ys zs == zip3 xs ys zs

--zip3'' :: [a] -> [b] -> [c] -> [(a,b,c)]
--zip3'' xs ys zs


-- Ex 7.24
qSort,dQSort,rQSort :: [Integer] -> [Integer]

-- normal qSort
qSort [] = []
qSort (x:xs)
  = qSort [y | y<-xs, y<=x] ++ [x] ++ qSort [y | y<-xs, y>x]

-- Just change the sence of the comparision operators
-- descending order qSort
dQSort []  = []
dQSort (x:xs)
  = dQSort [y | y<-xs, y>=x] ++ [x] ++ dQSort [y | y<-xs, y<x]

-- to remove items change "<=" to just "<" so equals are ignored
-- remove extra items
rQSort [] = []
rQSort (x:xs)
  = rQSort [y | y<-xs, y<x] ++ [x] ++ rQSort [y | y<-xs, y>x]


-- Ex 7.25  - sublist, subsequence

sublist :: String -> String -> Bool
sublist [] _    = True
sublist _  []   = False
sublist (x:xs) (y:ys)
  | x==y        = sublist xs ys
  | otherwise   = sublist (x:xs) ys

subsequence :: String -> String -> Bool
subsequence [] _  = True
subsequence _  [] = False
subsequence (x:xs) (y:ys)
  = (x==y && subsequence xs ys) ||
    (subsequence (x:xs) ys)


-- 7.6  Example: text processing

type Word = String
type Line = [Word]

whitespace :: [Char]
whitespace = ['\a','\t',' ']

lineLen :: Int
lineLen = 20

-- Extracting words

-- return a word separated by spaces from a given string
getWord :: String -> Word
getWord []  = []
getWord (x:xs)
  | elem x whitespace  = []
  | otherwise          = x : getWord xs

test_getWord  = TestList [
  TestCase (assertEqual "getword base case"     ""     (getWord "")),
  TestCase (assertEqual "getword start spaces"  ""     (getWord " boo")),
  TestCase (assertEqual "getword world"         "cat"  (getWord "cat")),
  TestCase (assertEqual "getword two words"     "cat"  (getWord "cat dog"))
  ]

-- drop the first word separated by spaces from a given string
dropWord  :: Word -> String
dropWord []  = []
dropWord (x:xs)
  | elem x whitespace   = (x:xs)
  | otherwise           = dropWord xs

test_dropWord = TestList [
  TestCase (assertEqual "dropWord base case"   ""      (dropWord "")),
  TestCase (assertEqual "dropWord start space" "  boo" (dropWord "  boo")),
  TestCase (assertEqual "dropWord one word"    ""      (dropWord "cat")),
  TestCase (assertEqual "dropWord two words"   " dog"  (dropWord "cat dog"))
  ]

-- remove beginning spaces from a given string
dropSpace  :: String -> String
dropSpace []  =[]
dropSpace (x:xs)
  | elem x whitespace  = dropSpace xs
  | otherwise          = (x:xs)

test_dropSpace = TestList [
  TestCase (assertEqual "dropSpace base case"   ""      (dropSpace "")),
  TestCase (assertEqual "dropSpace start space" "boo"   (dropSpace "  boo")),
  TestCase (assertEqual "dropSpace one word"    "cat"   (dropSpace "cat")),
  TestCase (assertEqual "dropSpace two words"   "cat dog" (dropSpace "cat dog"))
  ]


-- return a list of Words from a given string split at spaces
splitWords :: String -> [Word]
splitWords st = split (dropSpace st)

split :: String -> [Word]
split []  = []
split st
  = (getWord st) : split (dropSpace (dropWord st))

test_splitWords = TestList [
  TestCase (assertEqual "splitwords base case"   []
            (splitWords "")),
  TestCase (assertEqual "splitwords start space" ["boo"]
            (splitWords "  boo")),
  TestCase (assertEqual "splitwords one word"    ["cat"]
            (splitWords "cat")),
  TestCase (assertEqual "splitwords two words"   ["cat","dog"]
            (splitWords "cat dog")),
  TestCase (assertEqual "splitwords complex"     ["dog","cat"]
            (splitWords "  dog  cat"))
  ]



-- Splitting into worlds

-- return a line of a given length from a given string
getLine' :: Int -> [Word] -> Line
getLine' _ []        = []
getLine' len (w:ws)
  | length w <= len  = w : restOfLine
  | otherwise        = []
  where newLen       = len - (length w +1)
        restOfLine   = getLine' newLen ws

test_getLine' = TestList [
  TestCase (assertEqual "getLine' base case" [] (getLine' 20 [])),
  TestCase (assertEqual "getLine' " ["Mary","Poppins","looks"]
            (getLine' 20 ["Mary","Poppins", "looks","like"]))
  ]



-- produce a list filled lines given a string of text
splitLines :: [Word] -> [Line]
splitLines []  = []
splitLines ws
  = getLine' lineLen ws: splitLines (dropLine lineLen ws)

-- fill a text string into lines
fill :: String -> [Line]
fill = splitLines . splitWords


-- Exercises

-- Ex 7.27

-- remove a line from the front a list of words
dropLine :: Int -> [Word] -> Line
dropLine _ []        = []
dropLine len (w:ws)
  | length w <= len   = restOfLine
  | otherwise        = (w:ws)
  where newLen       = len - (length w + 1)
        restOfLine   = dropLine newLen ws

test_dropLine = TestList [
  TestCase (assertEqual "dropLine base case" [] (dropLine 20 [])),
  TestCase (assertEqual "dropLine " ["like","Marge"]
            (dropLine 20 ["Mary","Poppins","looks","like","Marge"]))
  ]


-- Ex 7.28

joinLine :: Line -> String
joinLine []     = ""
joinLine [w]    = w
joinLine (w:ws) = w ++ " " ++ joinLine ws

test_joinLine = TestList [
  TestCase (assertEqual "base" "" (joinLine [])),
  TestCase (assertEqual "one " "cat" (joinLine ["cat"])),
  TestCase (assertEqual "comples" "cat dog" (joinLine ["cat","dog"]))
  ]

-- Ex 7.29

joinLines :: [Line] -> String
joinLines []     = ""
joinLines (l:ls) = joinLine l ++ "\n" ++ joinLines ls

test_joinLines = TestList [
  TestCase (assertEqual "base" "" (joinLines [])),
  TestCase (assertEqual "one line" "cat dog\n" (joinLines [["cat","dog"]])),
  TestCase (assertEqual "two lines" "cat dog\nmouse cheese\n"
           (joinLines [["cat","dog"],["mouse","cheese"]]))
  ]

justify :: String -> String
justify = joinLines . fill


-- Ex 7.30  -- redefine using splitAt

--splitLine :: String -> [Line]
--splitLine [] =  []
--splitLine str = [chunk] ++ splitLine rem
--                where (_chunk,rem) = splitAt lineLen str
--                      chunk = "Hello"


--splitLength :: String -> (String,String)
--splitLenght = ()

-- Ex 7.31
