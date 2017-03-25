module Main where

import Text.Parsec

import Parsing.Parse
import Parsing.State
import Rendering.Render
import Rendering.RenderOptions

main :: IO ()
main = do
    renderOptions <- getRenderOptions
    input <- getContents
    let result = runParser ast initialState "" input
    either (putStrLn . show) (putStr . toHtml renderOptions) result
