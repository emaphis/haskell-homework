-- | Some testing

module Test where

--func :: Integer -> String -> [String]
--func n (x:xs) = map (x:) func (n-1) xs

double n = n + n
quadruple' n = double (double n)
quadruple = double . double
