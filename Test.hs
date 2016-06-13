module Main where

import Data.Either
import Text.Parsec

import Parse
import Render

main :: IO ()
main = do
    input <- getContents
    let result = parse ast "test" input
    putStrLn $ show result
    putStr $ either (\_ -> "") render result
