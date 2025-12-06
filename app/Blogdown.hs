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
    let result = runParser ast Parse.initialState "test" input
    putStr $ either show (toHtml renderOptions) result
