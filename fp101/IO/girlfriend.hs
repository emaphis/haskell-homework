-- | read girlfriend.txt

import System.IO

-- main = do
--   handle <- openFile "girlfriend.txt" ReadMode
--   contents <- hGetContents handle
--   putStr contents
--   hClose handle


-- using withFile

main =
  withFile "girlfriend.txt" ReadMode
    (\ handle ->
      do contents <- hGetContents handle
         putStr contents)

withFile' :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile' path mode f = do
  handle <- openFile path mode
  result <- f handle
  hClose handle
  return result

-- hGetcontents, hPutStr, hPutStrLn, hGetChar

-- readFile, writeFile, appendFile

main1 = do
  contents <- readFile "girlfriend.txt"
  putStr contents
