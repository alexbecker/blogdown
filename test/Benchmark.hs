module Main where

import Criterion.Main
import Data.Either
import Data.String.Utils (replace)

import Parsing.Parse (parse)
import Parsing.ParseOptions (defaultParseOptions)
import Rendering.Render (toHtml)
import Rendering.RenderOptions (RenderOptions, defaultRenderOptions, inlineCSS, inlineJS)

parseAndRender :: RenderOptions -> String -> String
parseAndRender options = either show (toHtml options) . parse defaultParseOptions

main :: IO ()
main = do
    readme <- readFile "Readme.md"
    let readmeWithErrors = replace "](" "]" readme
    defaultMain [bench "Readme.md" $ nf (parseAndRender defaultRenderOptions) readme,
                 bench "Readme.md with inlining" $ nf (parseAndRender defaultRenderOptions {inlineJS=True, inlineCSS=True}) readme,
                 bench "Readme.md with errors" $ nf (parseAndRender defaultRenderOptions) readmeWithErrors]
