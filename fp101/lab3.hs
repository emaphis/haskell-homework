module Lab3 where

-----------------------------------------------------------------------------------------------------------------------------
-- LIST COMPREHENSIONS
------------------------------------------------------------------------------------------------------------------------------

-- ===================================
-- Ex. 0 - 2
-- ===================================

evens :: [Integer] -> [Integer]
evens xs = [x | x <- xs, x `mod` 2 == 0]

-- sum . evens $ [827305 .. 927104] => 43772529500
-- sum . evens $ []   => 0

-- =================================
-- Ex. 3 - 4
-- ===================================

-- complete the following line with the correct type signature for this function


squares :: Integer -> [Integer]
squares n = [x*x | x <- [1..n]]

sumSquares :: Integer -> Integer
sumSquares n = sum (squares n)

-- sumSquares 50   => 42925

-- ===================================
-- Ex. 5 - 7
-- ===================================

-- complete the following line with the correct type signature for this function
-- squares' :: ...
squares' :: Integer -> Integer -> [Integer]
squares' m n = [x*x | x <- [(n+1) .. (m+n)]]

sumSquares' :: Integer -> Integer
sumSquares' x = sum . uncurry squares' $ (x, x)

-- sumSquares' 50       => 295425
-- sum $ squares' 10 0  => 385
-- sum $ squares' 0 10  => 0


-- ===================================
-- Ex. 8
-- ===================================

coords :: Integer -> Integer -> [(Integer,Integer)]
coords m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- > coords 1 1  => [(0,0),(0,1),(1,0),(1,1)]
-- > coords 1 2  => [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]

-- foldr (-) 0 . map (uncurry (*)) $ coords 5 7
-- => -60

-- Ex (9)
