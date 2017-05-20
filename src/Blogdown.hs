module Main where

import Options
import Parsing.Parse
import Rendering.Render

main :: IO ()
main = do
    (parseOptions, renderOptions) <- getOptions
    input <- getContents
    either (putStrLn . show) (putStr . toHtml renderOptions) $ parse parseOptions input
