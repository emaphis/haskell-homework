-- | append input to the todo.txt file
-- | demo appendFile

--import System.IO

main = do
  todoItem <- getLine
  appendFile "todo.txt" (todoItem ++ "\n")

