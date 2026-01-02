module Main where

import Options
import Parsing.Parse
import Rendering.Render
import System.Exit (exitFailure)

main :: IO ()
main = do
    (parseOptions, renderOptions) <- getOptions
    input <- getContents
    case parse parseOptions input of
        Left err -> do
            putStrLn $ show err
            exitFailure
        Right ast -> putStr $ toHtml renderOptions ast
