-- | Data types, tuples, and lists

module Chapter5 where

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


-- Patern matiching:
-- funtions over tuples are usually defined using patern matching

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

maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer,Integer)
maxThreeOccurs x y z = (max (fst m) z, n)
  where m = maxOccurs x y
        n = if z == fst m
            then 1 + snd m
            else snd m
-- 1 2 3, 3 2 1, 2 3 1, 2 3 3, 3 3 3


-- Ex 5.2 - orderTriple -
-- returns a triple in assending order given a triple

orderTriple :: (Integer,Integer,Integer) -> (Integer,Integer,Integer)
orderTriple (m, n, o) = undefined
