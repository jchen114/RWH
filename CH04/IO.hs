import System.Environment (getArgs)
import Data.Maybe
  
safeHead' :: [a] -> Maybe a
safeHead' [] = Nothing
safeHead' (a:as) = Just a

printFirstWordOfLine :: String -> String
printFirstWordOfLine str = unwords $ catMaybes $ map safeHead' $ map words $ lines str

interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
           [input,output] -> interactWith function input output
           _ -> putStrLn "error: exactly two arguments needed"
        -- replace "id" with the name of our function below
        myFunction = printFirstWordOfLine
