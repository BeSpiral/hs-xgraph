module Lib
    ( someFunc, greet
    ) where

someFunc :: IO ()
someFunc = putStrLn "Ho ho ho!"

greet :: String -> String
greet name = "Hello " ++ name ++ "!"
