module RenderOptions where

import System.Environment

data RenderOptions = RenderOptions {
    footnotePrefix :: String,
    footnoteIndexFrom :: Int,
    footnoteBacklinks :: Bool
}

defaultRenderOptions = RenderOptions {
    footnotePrefix = "",
    footnoteIndexFrom = 0,
    footnoteBacklinks = False
}

renderOptions :: [String] -> RenderOptions
renderOptions [] = defaultRenderOptions
renderOptions (key : ls) = case key of
    "--footnote-prefix" -> (renderOptions $ tail ls) {footnotePrefix=head ls}
    "--footnote-index-from" -> (renderOptions $ tail ls) {footnoteIndexFrom=read $ head ls}
    "--footnote-backlinks" -> (renderOptions ls) {footnoteBacklinks=True}

getRenderOptions :: IO RenderOptions
getRenderOptions = fmap renderOptions getArgs
