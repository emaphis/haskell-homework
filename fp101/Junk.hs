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


unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
  | p x        = []
  | otherwise  = h x : unfold p h t (t x)


type Bit = Integer
int2bin :: Integer -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

int2bin' :: Integer -> [Integer]
int2bin' = unfold (==0) (`mod` 2) (`div` 2)


-- unReturn :: Monad m => m a -> a
-- unReturn m = do _ <- putStr "hell0"
--                 a <- m
--                 return a

fn :: a -> b
fn  = \ a -> undefined
