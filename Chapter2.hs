-- 	Haskell: The Craft of Functional Programming
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 2011.
--
-- 	Chapter 2
--
-- 	The example script FirstScript.hs is provided separately,
--      as are the Pictures.hs and PicturesSVG.hs modules.
--
------------------------------------------------------------------------------

-- One Comment  - one line only

{-
   Two Commnet
   Multi-line
-}

module Chapter2 where


cube :: Integer -> Integer
cube n = n*n*n

fac1 :: Integer -> Integer
fac1 0 = 1
fac1 n = n * fac1 (n-1)

-- 1 1 2 3 5 8 13 21 34 55 89 144 ...
fibo :: Integer -> Integer
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)
