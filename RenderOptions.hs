module RenderOptions where

import System.Environment

data RenderOptions = RenderOptions {
    footnotePrefix :: String,
    footnoteIndexFrom :: Int,
    footnoteBacklinks :: Bool,
    emDashes :: Bool,
    inlineCSS :: Bool,
    inlineJS :: Bool
}

defaultRenderOptions = RenderOptions {
    footnotePrefix = "",
    footnoteIndexFrom = 0,
    footnoteBacklinks = False,
    emDashes = False,
    inlineCSS = False,
    inlineJS = False
}

renderOptions :: [String] -> RenderOptions
renderOptions [] = defaultRenderOptions
renderOptions (key : ls) = case key of
    "--footnote-prefix" -> (renderOptions $ tail ls) {footnotePrefix=head ls}
    "--footnote-index-from" -> (renderOptions $ tail ls) {footnoteIndexFrom=read $ head ls}
    "--footnote-backlinks" -> (renderOptions ls) {footnoteBacklinks=True}
    "--em-dashes" -> (renderOptions ls) {emDashes=True}
    "--inline-css" -> (renderOptions ls) {inlineCSS=True}
    "--inline-js" -> (renderOptions ls) {inlineJS=True}

getRenderOptions :: IO RenderOptions
getRenderOptions = fmap renderOptions getArgs
