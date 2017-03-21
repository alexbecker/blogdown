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
    let result = runParser ast Parse.initialState "" input
    either (putStrLn . show) (putStr . toHtml renderOptions) result
