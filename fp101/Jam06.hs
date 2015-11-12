-- | Higher Order Functions - Church numerals
-- | Eddy Bertoluzzzo

module Jam06 where

type Church a = (a -> a) -> a -> a


zero,one,two,three,five,six :: Church a
zero = \s z -> z

one  = \s z -> s z

--two'' = \s z -> s (s z)

-- two'  = \s z -> (s . s) z

two   = \s  -> s . s  -- Eta reduction

five  = \s  -> s . s . s . s .s

-- Church to Int
c2i :: Church Int  -> Int
c2i x = x (+1) 0

-- c2i two = (\s z -> s (s z)) (+1) 0
--         = (+1) ((+1) 0)

c2s x = x ('*':) ""

-- c2s two
-- (\s z -> s (s z)) ('*':) ""
-- ('*':) ('*':"")
-- '*':'*'  => "**"

{-
x' = c2i x
y' = c2i y

x' + y' = c2i (add x y)
x' + y' = c2i x + c2i y
        = x (+1) 0 + c2i y
        = x (+1) (c2i y)
        = x (+1) (y (+1) 0)
        = (\s z -> x s (y s z)) (+1) 0
        = (add x y) (+1) 0
        = c2i (add x y)
-}
add :: Church a  -> Church a  -> Church a
add x y = \s z -> x s (y s z)       

-- two = \s -> s . s
three = \f -> f . f . f
six   = \s -> (s . s) . (s . s) . (s . s)

-- mul'' x y = \s z -> x (y s) z

-- mul' x y  = \s z -> (x . y) s z

mul :: Church a -> Church a -> Church a
mul x y   = x . y
