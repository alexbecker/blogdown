module Main where

import Data.Either
import Text.Parsec

import Parse
import Render

main :: IO ()
main = do
    line <- getLine
    let result = parse html "test" line
    putStrLn $ either (\_ -> "") toHtml result
