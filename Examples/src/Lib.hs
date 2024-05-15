module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
            putStrLn "Hello World"
            putStrLn "Like everything else now"
            putStrLn "Enter: "
            xs <- getLine
            putStr "The string is"
            putStr (show (length xs))
            putStr " Characters long"
