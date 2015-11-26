-- | Hello world program

import Data.Char

main :: IO ()
main = do putStrLn "What's your name first name?"
          firstName <- getLine
          putStrLn "What's you last name?"
          lastName <- getLine
          let bigFirstName = map toUpper firstName
              bigLastName  = map toUpper lastName
          putStrLn ("Hay " ++ bigFirstName ++ " " ++ bigLastName ++  ", you rock!")


