module Main where

import Data.Either
import Text.Parsec

import Parse
import Render
import RenderOptions

main :: IO ()
main = do
    renderOptions <- getRenderOptions
    input <- getContents
    let result = parse ast "test" input
    putStrLn $ show result
    putStr $ either (\_ -> "") (render renderOptions) result
