module Main where

import Criterion.Main
import Data.Either
import Data.String.Utils (replace)

import Parsing.Parse
import Rendering.Render
import Rendering.RenderOptions

parseAndRender :: RenderOptions -> String -> String
parseAndRender options = either show (toHtml options) . parse

main :: IO ()
main = do
    readme <- readFile "Readme.md"
    let readmeWithErrors = replace "](" "]" readme
    defaultMain [bench "Readme.md" $ nf (parseAndRender defaultRenderOptions) readme,
                 bench "Readme.md with inlining" $ nf (parseAndRender defaultRenderOptions {inlineJS=True, inlineCSS=True}) readme,
                 bench "Readme.md with errors" $ nf (parseAndRender defaultRenderOptions) readmeWithErrors]
