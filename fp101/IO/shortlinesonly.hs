-- | print out shortlines only

-- main = do
--   contents <- getContents
--   putStr (shortLinesOnly contents)

-- common pattern
main = interact shortLinesOnly


shortLinesOnly :: String -> String
shortLinesOnly input =
  let allLines = lines input
      shortLines = filter (\line -> length line < 10) allLines
      result = unlines shortLines
  in result

