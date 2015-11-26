-- | solve palindrome with interact

respondPalindromes :: String -> String
respondPalindromes contents = unlines (map (\xs -> if isPalindrome xs then "palindrome" else "not a palindome") (lines contents))
  where isPalindrome xs = xs == reverse xs


main = interact respondPalindromes

