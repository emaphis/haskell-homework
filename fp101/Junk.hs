module Junk where

--import Char (isDigit)


exchange :: Integral a => a -> a
exchange x =
  b*10 + a
  where a = x `div` 10
        b = x `mod` 10

is_valid_asnwer :: Integral a => (a, a) -> Bool
is_valid_asnwer (gfata,gsona) =
  (gfata == gsona * 4) && exchange gsona == exchange gfata * 3

find :: Integral t => (t, t) -> [(t, t)]
find (max, min) =
   [(x,y) | x <- [min..max], y <- [x..max], y>x && is_valid_asnwer(y,x)]

-- 72,18
test1 max min =
  [(x,y) | x <- [min..max], y<-[x..max], is_valid_asnwer(x,y)]

test2 = [(x,y) | x <- [1..6], y<-[x..6], x<y]


n = a `div` length xs
    where
      a=10
      xs=[1,2,3,4,5]

-- More Junk

pair :: a -> b -> (a,b)
pair x y = (x, y)

pair3 :: a -> (Int, a)
pair3 = pair 3


data Expr = Val Int | Div Expr Expr

safediv n m =  if m == 0 then Nothing else Just (n `div` m)

eval (Val n)   = Just n
eval (Div x y) = do n <- eval x
                    m <- eval y
                    safediv n m
