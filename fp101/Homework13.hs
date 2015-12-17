-- | Chapter 13 - Jolly presents

module Homework13 where


foldl0,foldl2,foldl3,foldl4
  :: (b -> a -> b) -> b -> [a] -> b

-- compiles
foldl0 f a bs = foldr (\b -> \g -> (\a -> g (f a b))) id bs a 

-- compiles
foldl2 f a bs = foldr (\a b -> f b a) a bs

-- compiles
foldl3 f = flip $ foldr (\a b g -> b (f g a)) id

-- compiles
foldl4 = foldr . flip
