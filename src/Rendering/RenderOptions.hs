{-# LANGUAGE DeriveGeneric #-}

module Rendering.RenderOptions where

import GHC.Generics (Generic)
import System.Environment

data RenderOptions = RenderOptions {
    footnotePrefix :: String,
    footnoteIndexFrom :: Int,
    footnoteBacklinks :: Bool,
    emDashes :: Bool,
    inlineCSS :: Bool,
    inlineJS :: Bool,
    allowedTags :: Maybe [String],
    allowedAttributes :: Maybe [String]
} deriving (Generic)

defaultRenderOptions = RenderOptions {
    footnotePrefix = "",
    footnoteIndexFrom = 0,
    footnoteBacklinks = False,
    emDashes = False,
    inlineCSS = False,
    inlineJS = False,
    allowedTags = Nothing,
    allowedAttributes = Nothing
}

splitCommas :: String -> [String]
splitCommas "" = []
splitCommas s = takeWhile (/= ',') s : splitCommas (dropWhile (/= ',') s)

renderOptions :: [String] -> RenderOptions
renderOptions [] = defaultRenderOptions
renderOptions (key : ls) = case key of
    "--footnote-prefix" -> (renderOptions $ tail ls) {footnotePrefix=head ls}
    "--footnote-index-from" -> (renderOptions $ tail ls) {footnoteIndexFrom=read $ head ls}
    "--footnote-backlinks" -> (renderOptions ls) {footnoteBacklinks=True}
    "--em-dashes" -> (renderOptions ls) {emDashes=True}
    "--inline-css" -> (renderOptions ls) {inlineCSS=True}
    "--inline-js" -> (renderOptions ls) {inlineJS=True}
    "--allowed-tags" -> (renderOptions $ tail ls) {allowedTags=Just $ splitCommas $ head ls}
    "--allowed-attributes" -> (renderOptions $ tail ls) {allowedAttributes=Just $ splitCommas $ head ls}

getRenderOptions :: IO RenderOptions
getRenderOptions = fmap renderOptions getArgs
