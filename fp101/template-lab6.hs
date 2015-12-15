------------------------------------------------------------------------------------------------------------------------------
-- ROSE TREES, FUNCTORS, MONOIDS, FOLDABLES
------------------------------------------------------------------------------------------------------------------------------

module Rose where

data Rose a = a :> [Rose a] deriving Show

-- ===================================
-- Ex. 0-2
-- ===================================

root :: Rose a -> a
root (y :> _) =  y

children :: Rose a -> [Rose a]
children (_ :> ys) = ys

tree0,tree1 :: Rose Char

tree0 = 'x' :> map (flip (:>) []) ['a'..'x']

len0,len1 :: Int

len0 = length $ children tree0  -- 24

tree1 = 'x' :> map (\c -> c :> []) ['a'..'A']
len1 = length (children tree1)  -- 0

xs :: Rose Integer
xs = 0 :> [1 :> [2 :> [3 :> [4 :> [], 5 :> []]]], 6 :> [], 7 :> [8 :> [9 :> [10 :> []], 11 :> []], 12 :> [13 :> []]]]

ex2 :: Integer
ex2 = root . head . children . head . children . head . drop 2 $ children xs

-- ===================================
-- Ex. 3-7
-- ===================================

size :: Rose a -> Int
size (_ :> []) = 1
size (_ :> rs) = 1 + sum (map size rs)

leaves :: Rose a -> Int
leaves (_ :> []) = 1
leaves (_ :> rs) = sum (map leaves rs)

tree3 :: Rose Integer
tree3 = 1 :> map (\c -> c :> []) [1..5]

int4,int5,int6,int8 :: Int
int4 = size . head . children $ tree3

int5 = leaves tree3

int6 = product (map leaves (children tree3))

ex7 :: Int
ex7 = (*) (leaves . head . children . head . children $ xs) (product . map size . children . head . drop 2 . children $ xs)

-- ===================================
-- Ex. 8-10
-- ===================================

-- fmap :: (a -> b) -> f a -> f b
instance Functor Rose where
  fmap f (r :> []) = f r :> []
  fmap f (r :> rs) = f r :> map (fmap f) rs

int8 = size (fmap leaves (fmap (:> []) tree3))

f9 :: Rose a -> Rose a
f9 r = fmap head $ fmap (\x -> [x]) r

ex10 :: Integer
ex10 = round . root . head . children . fmap (\x -> if x > 0.5 then x else 0) $ fmap (\x -> sin(fromIntegral x)) xs

-- ===================================
-- Ex. 11-13
-- ===================================

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m

newtype Sum a = Sum a
newtype Product a = Product a

instance Num a => Monoid (Sum a) where
  mempty = Sum 0  -- type constructor 'Sum'
  mappend (Sum x) (Sum y) = Sum (x+y)

instance Num a => Monoid (Product a) where
  mempty = Product 1
  mappend (Product x) (Product y) = Product (x*y)

unSum :: Sum a -> a
unSum (Sum x) = x
unProduct :: Product a -> a
unProduct (Product x) = x

int11 = unProduct (Product 6 `mappend` (Product . unSum $ Sum 3 `mappend` Sum 4))

--exp12 :: Sum Integer
exp12 :: Num string => Sum string
exp12 =  Sum 3 `mappend` Sum 4


num1,num2 :: Sum Integer

num1 = mappend (mappend (Sum 2) (mappend (mappend mempty (Sum 1)) mempty)) (mappend (Sum 2) (Sum 1))

num2 = mappend (Sum 3) (mappend mempty (mappend (mappend (mappend (Sum 2) mempty) (Sum (-1))) (Sum 3)))


ex13 :: Integer
ex13 = unSum (mappend (Sum 5) (Sum (unProduct (mappend (Product (unSum num2)) (mappend (Product (unSum num1)) (mappend mempty (mappend (Product 2) (Product 3))))))))

-- ===================================
-- Ex. 14-15
-- ===================================

class Functor f => Foldable f where
  fold :: Monoid m => f m -> m
  foldMap :: Monoid m => (a -> m) -> (f a -> m)
  foldMap f ys = fold (fmap f ys)

instance Foldable [] where
  fold = foldr mappend mempty

instance Foldable Rose where
  fold (r :> []) = mappend r mempty
  fold (r :> rs) = mappend r (fold (map fold rs))

tree14 :: Rose Integer
tree14 = 1 :> [2 :> [], 3 :> [4 :> []]]

tree' = fmap Product tree14

int14 = unProduct $ fold tree'

sumxs :: Rose (Sum Integer)
sumxs = Sum 0 :> [Sum 13 :> [Sum 26 :> [Sum (-31) :> [Sum (-45) :> [], Sum 23 :> []]]], Sum 27 :> [], Sum 9 :> [Sum 15 :> [Sum 3 :> [Sum (-113) :> []], Sum 1 :> []], Sum 71 :> [Sum 55 :> []]]]

ex15 :: Integer
ex15 = unSum (mappend (mappend (fold sumxs) (mappend (fold . head . drop 2 . children $ sumxs) (Sum 30))) (fold . head . children $ sumxs))

-- ===================================
-- Ex. 16-18
-- ===================================

tree16 = 42 :> [3 :> [2:> [], 1 :> [0 :> []]]]

int16 = unSum $ foldMap Sum tree16

ex17,ex18 :: Integer

ex17 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (mappend (foldMap (\x -> Sum x) . head . drop 2 . children $ xs) (Sum 30))) (foldMap (\x -> Sum x) . head . children $ xs))

ex18 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (Sum (unProduct (mappend (foldMap (\x -> Product x) . head . drop 2 . children $ xs) (Product 3))))) (foldMap (\x -> Sum x) . head . children $ xs))

-- ===================================
-- Ex. 19-21
-- ===================================

fproduct, fsum :: (Foldable f, Num a) => f a -> a
fsum n = unSum (foldMap Sum n)
fproduct n = unProduct (foldMap Product n)

ex21 :: Integer
ex21 = ((fsum . head . drop 1 . children $ xs) + (fproduct . head . children . head . children . head . drop 2 . children $ xs)) - (fsum . head . children . head . children $ xs)
