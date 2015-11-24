-- | Monad experiments
-- | http://www.cs.nott.ac.uk/~pszgmh/monads

module Monads where
import Prelude hiding ((>>=),Maybe,Just,Nothing)

-- Abstracting programming patterns

inc          :: [Int] -> [Int]
inc []        = []
inc (n:ns)    = n+1 : inc ns

sqr          :: [Int] -> [Int]
sqr []        = []
sqr (n:ns)    = n^2 : sqr ns


-- extract out the pattern:
map'         :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map f xs

inc'  = map' (+1)
sqr'  = map' (^2)


-- a simple eveluator

data Expr = Val Int | Div Expr Expr

eval1           :: Expr -> Int
eval1 (Val n)    = n
eval1 (Div  x y) = eval1 x `div` eval1 y


-- Take care of division by zero
data Maybe a = Nothing | Just a

safediv        :: Int -> Int -> Maybe Int
safediv n m     = if n==0
                  then Nothing
                  else Just (n `div` m)

eval2           :: Expr -> Maybe Int
eval2 (Val n)    = Just n
eval2 (Div x y)  = case eval2 x of
                     Nothing -> Nothing
                     Just n   -> case eval2 y of
                                   Nothing -> Nothing
                                   Just m -> safediv n m

-- abstracting out the case analyses

seqn                   :: Maybe a -> Maybe b -> Maybe (a,b)
seqn Nothing _          = Nothing
seqn _        Nothing   = Nothing
seqn (Just x) (Just y)  = Just (x,y)


eval3            :: Expr -> Maybe Int
eval3 (Val n)     = Just n
eval3 (Div x y)   = apply1 f (eval3 x `seqn` eval3 y)
                      where f (n,m)  = safediv n m

apply1            :: (a -> Maybe b) -> Maybe a -> Maybe b
apply1 _ Nothing    = Nothing
apply1 f (Just x)   = f x


-- combining sequencing and processing

(>>=)             :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing  >>= _     = Nothing
(Just x) >>= f     = f x


eval4             :: Expr -> Maybe Int
eval4 (Val n)      = Just n
eval4 (Div x y)    = eval4 x >>= (\n ->
                     eval4 y >>= (\m ->
                      safediv n m))

-- eval using 'do'

--eval5            :: Expr -> Maybe Int
eval5 (Val n)     = Just n
eval5 (Div x y)   = do n <- eval5 x
                       m <- eval5 y
                       safediv n m


-- Monads in Haskell

class Monad1 m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

instance Monad1 Maybe where
  return x     = Just x

  Nothing  >>= _  = Nothing
  (Just x) >>= f  = f x
