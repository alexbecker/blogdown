module Main where

import Data.Either
import Text.Parsec

import Parse
import Render

main :: IO ()
main = do
    line <- getLine
    let result = parse ast "test" line
    putStrLn $ show result
    putStr $ either (\_ -> "") toHtml result
