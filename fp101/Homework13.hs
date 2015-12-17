-- | Chapter 13 - Jolly presents

module Homework13 where


foldl0,foldl2,foldl3,foldl4
  :: (b -> a -> b) -> b -> [a] -> b

-- good 57
foldl0 f a bs = foldr (\b -> \g -> (\a -> g (f a b))) id bs a 
int0 = foldl0 f 0 [1..5] -- 57

-- bad 129
foldl2 f a bs = foldr (\a b -> f b a) a bs
int2 = foldl2 f 0 [1..5] -- 129

-- good 57
foldl3 f = flip $ foldr (\a b g -> b (f g a)) id
int3 = foldl3 f 0 [1..5] -- 57

-- bad 129
foldl4 = foldr . flip
int4 = foldl4 f 0 [1..5] -- 129


-- test function - non commutative:
f :: Int -> Int -> Int
f x y = 2*x + y

-- so:
n1 = foldr f 0 [1..5] -- = 30
n2 = foldl f 0 [1..5] -- = 57
