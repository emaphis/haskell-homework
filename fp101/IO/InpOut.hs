-- | IO Experiments from LYAH
-- | http://learnyouahaskell.com/input-and-output

module InpOut where


putStr'     :: String -> IO ()
putStr' []   = return ()
putStr' (x:xs) = do
  putChar x
  putStr' xs

-- print = putStrLn . show

