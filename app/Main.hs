module Main where

import Lib
import SimpleGraph
import System.IO (readFile)
import Data.Time (getCurrentTime)

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn (greet "Jim")
  putStrLn "It is a very nice day!"
  putStrLn "At the tone, the time is:"
  printTime
  putStrLn "\n"

-- greet "Jim"

printConfig = do
  contents <- readFile "stack.yaml"
  putStrLn contents

printTime = do
  time <- getCurrentTime
  putStrLn (show time)
