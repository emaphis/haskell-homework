-- | read girlfriend.txt then output in caps
-- | demo writFile

import System.IO
import Data.Char

main = do
  contents <- readFile "girlfriend.txt"
  writeFile "girlfriendcaps.txt" (map toUpper contents)

