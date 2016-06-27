module RenderOptions where

import System.Environment

data RenderOptions = RenderOptions {
    footnotePrefix :: String,
    footnoteIndexFrom :: Int
}

defaultRenderOptions = RenderOptions {
    footnotePrefix = "",
    footnoteIndexFrom = 0
}

renderOptions :: [String] -> RenderOptions
renderOptions [] = defaultRenderOptions
renderOptions (key : (value : ls)) = case key of
    "--footnote-prefix" -> (renderOptions ls) {footnotePrefix=value}
    "--footnote-index-from" -> (renderOptions ls) {footnoteIndexFrom=read value}

getRenderOptions :: IO RenderOptions
getRenderOptions = fmap renderOptions getArgs
