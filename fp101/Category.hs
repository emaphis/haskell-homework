-- | Functors, Applicatives, Monoids, Monads, oh yeah.

module Category where


-- Functors

class Functor1 f where
  fmap1 :: (a -> b) -> f a -> f b

instance Functor1 [] where
  fmap1 = map

lst1 = fmap (*2) [1,2,3]
-- should equal
lst2 = map (*2) [1,2,3]

data  Maybe1 a = Just1 a | Nothing1
               deriving (Eq,Ord,Read,Show)

instance Functor1 Maybe1 where
  fmap1 f (Just1 x) = Just1 (f x)
  fmap1 f Nothing1 = Nothing1


data Either1 a b = Left1 a | Right1 b
                 deriving (Eq,Ord,Read,Show)

instance Functor1 (Either1 a) where
  fmap1 f (Right1 x) = Right1 (f x)
  fmap1 f (Left1 x)  = Left1 x


-- Kinds:
class Tofu t where
  tofu :: j a -> t a j

data Frank a b = Frank {frankField :: b a}
               deriving (Show)

frnk1 = Frank {frankField = Just "HaHa"}
frnk2 = Frank {frankField = "YES"}

instance Tofu Frank where
  tofu x = Frank x

tofu1 = tofu (Just 'a') :: Frank Char Maybe
tofu2 = tofu ["HELLO"] :: Frank [Char] []

-- more type fy

data Barry t k p = Barry {yabba :: p, dabba :: t k}

instance Functor (Barry a b) where
  fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}
