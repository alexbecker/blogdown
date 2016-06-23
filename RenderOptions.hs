module RenderOptions where

import System.Environment

data RenderOptions = RenderOptions {
    footnotePrefix :: String
}

defaultRenderOptions = RenderOptions {
    footnotePrefix = ""
}

renderOptions :: [String] -> RenderOptions
renderOptions [] = defaultRenderOptions
renderOptions (key : (value : ls)) = case key of
    "--footnote-prefix" -> (renderOptions ls) {footnotePrefix=value}

getRenderOptions :: IO RenderOptions
getRenderOptions = fmap renderOptions getArgs
