{- #########################################################

        FirstScript.hs
        Simon Thompson, August 2010.

######################################################### -}

module FirstScript where

--      The value size is an integer (Integer), defined to be
--      the sum of twelve and thirteen.

size :: Integer
size = 12+13

--      The function to square an integer.

square :: Integer -> Integer
square n = n*n

--      The function to double an integer.

double :: Integer -> Integer
double n = 2*n

--      An example using double, square and size.

example :: Integer
example = double (size - square (2+2))  -- 18

-- Task 4 from page 33

task4a :: Integer -> Integer
task4a n = square (double n)

task4b :: Integer -> Integer
task4b n = double (square n)

a :: Integer
a = 23

fac 0 = 1
fac n = n * fac (n-1)
