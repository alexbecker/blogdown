module Main where

import Parsing.Parse
import Rendering.Render
import Rendering.RenderOptions

main :: IO ()
main = do
    renderOptions <- getRenderOptions
    input <- getContents
    either (putStrLn . show) (putStr . toHtml renderOptions) $ parse input
