-- | Church numerals

module Church where

type Church a = (a -> a) -> a -> a

zero,one,two,three,four,five,six :: Church a

zero = \s z -> z
one  = \s z -> s z
two   = \s  -> s . s
three = \s  -> s . s . s
four  = \s  -> s . s . s. s
five  = \s  -> s . s . s . s . s
six   = \s  -> s . s . s . s . s . s

-- Church to Int
c2i :: Church Int  -> Int
c2i x = x (+1) 0

-- Church to String
c2s :: Church String -> String
c2s x = x ('*':) ""

add :: Church a  -> Church a  -> Church a
add x y = \s z -> x s (y s z)

mul :: Church a -> Church a -> Church a
mul x y   = x . y

i2c :: Int -> Church a
i2c n
  | n==0      = \s z -> z
  | otherwise = add (\s -> s) (i2c (n-1))

i2c' :: Int -> Church a
i2c' n
  | n==0      = zero
  | otherwise = add one (i2c (n-1))


inc :: Church a -> Church a
inc = add one



foldr2       :: (a -> b -> b) -> b -> [a] -> b
foldr2 f v []     = v
foldr2 f v (x:xs) = f x (foldr2 f v xs)


evsl :: Num b => [b] -> b
evsl  = foldl (\x y -> y + (10 * x)) 0

--evsl1 :: Num b => [b] -> b
--eval1 []  = 0
--eval1 xs  =

fun :: Num a => a -> a -> a
fun x y = y + (10 * x)



doubleAdd :: Num a => (a -> (a -> (a -> a)))
doubleAdd x y z = x + y +  z
