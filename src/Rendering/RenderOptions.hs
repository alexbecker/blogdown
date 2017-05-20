{-# LANGUAGE DeriveGeneric #-}

module Rendering.RenderOptions where

import qualified Data.Map.Strict as M
import GHC.Generics (Generic)

data RenderOptions = RenderOptions {
    footnotePrefix :: String,
    footnoteIndexFrom :: Int,
    footnoteBacklinks :: Bool,
    emDashes :: Bool,
    inlineCSS :: Bool,
    inlineJS :: Bool,
    allowedTags :: Maybe [String],
    allowedAttributes :: Maybe [String]
} deriving (Show, Generic)

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

renderOptions :: M.Map String ([String] -> RenderOptions -> RenderOptions)
renderOptions = M.fromList([
    ("--footnote-prefix", \[x] -> \r -> r {footnotePrefix=x}),
    ("--footnote-index-from", \[x] -> \r -> r {footnoteIndexFrom=read x}),
    ("--footnote-backlinks", \[] -> \r -> r {footnoteBacklinks=True}),
    ("--em-dashes", \[] -> \r -> r {emDashes=True}),
    ("--inline-css", \[] -> \r -> r {inlineCSS=True}),
    ("--inline-js", \[] -> \r -> r {inlineJS=True}),
    ("--allowed-tags", \[x] -> \r -> r {allowedTags=Just $ splitCommas x}),
    ("--allowed-attributes", \[x] -> \r -> r {allowedAttributes=Just $ splitCommas x})])
