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

getSingleArg :: [String] -> String
getSingleArg [] = ""
getSingleArg (x : xs) = x

renderOptions :: M.Map String ([String] -> RenderOptions -> RenderOptions)
renderOptions = M.fromList([
    ("--footnote-prefix", \xs -> \r -> r {footnotePrefix=getSingleArg xs}),
    ("--footnote-index-from", \xs -> \r -> r {footnoteIndexFrom=read $ getSingleArg xs}),
    ("--footnote-backlinks", \_ -> \r -> r {footnoteBacklinks=True}),
    ("--em-dashes", \_ -> \r -> r {emDashes=True}),
    ("--inline-css", \_ -> \r -> r {inlineCSS=True}),
    ("--inline-js", \_ -> \r -> r {inlineJS=True}),
    ("--allowed-tags", \xs -> \r -> r {allowedTags=Just $ splitCommas $ getSingleArg xs}),
    ("--allowed-attributes", \xs -> \r -> r {allowedAttributes=Just $ splitCommas $ getSingleArg xs})])
