module Main where

import Lib
import Parser
import System.Environment (getArgs)

main :: IO ()
-- main = putStrLn "Hello, Haskell!"
main = do 
    [a] <- getArgs
    parseFile a >>= print