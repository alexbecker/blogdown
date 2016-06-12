module Render where

import Data.List
import Data.Maybe
import Text.Parsec.Char

import AST

class ToHtml a where
    toHtml :: a -> String

instance ToHtml AST where
    toHtml (AST bs) = unlines $ map toHtml bs

withTag :: String -> String -> String
withTag tag content = "<" ++ tag ++ ">" ++ content ++ "</" ++ tag ++ ">"

showAttrs :: [(String, String)] -> String
showAttrs [] = ""
showAttrs ((name, val) : as) = name ++ "=\"" ++ val ++ "\"" ++ spacing ++ showAttrs as
    where
        spacing = if null as then "" else " "

withTagAttrs :: String -> [(String, String)] -> String -> String
withTagAttrs tag attrs content = "<" ++ tag ++ " " ++ showAttrs attrs ++ ">" ++ content ++ "</" ++ tag ++ ">"

fancyUnlines :: [String] -> String
fancyUnlines = concat . intersperse "\n"

instance ToHtml Block where
    toHtml (Paragraph ls) = withTag "p" $ fancyUnlines $ map toHtml ls
    toHtml (Header level text) = withTag ("h" ++ show level ) $ toHtml text
    toHtml (UnorderedList ls) = withTag "ul" $ unlines $ map (withTag "li" . toHtml) ls
    toHtml (BlockQuote ls) = withTag "blockquote" $ fancyUnlines $ map toHtml ls
    toHtml (BlockCode s) = withTag "pre" $ withTag "code" $ unlines s
    toHtml (FootnoteDef identifier ls) = withTagAttrs "p" [("id", "footnote-" ++ identifier)] $ fancyUnlines $ map toHtml ls

instance ToHtml Line where
    toHtml (Line is) = concatMap toHtml is

instance ToHtml Inline where
    toHtml (InlineLink l) = toHtml l
    toHtml (InlineNonLink s) = toHtml s

instance ToHtml LinkContents where
    toHtml (Italics s) = withTag "i" s
    toHtml (Bold s) = withTag "b" s
    toHtml (Code s) = withTag "code" s
    toHtml (FootnoteRef identifier) = withTag "sup" $ withTagAttrs "a" [("href", "#footnote-" ++ identifier)] ("[" ++ "0" ++ "]")   -- TODO: implement numbering
    toHtml (Plaintext s) = s
    toHtml (Html' h) = toHtml h

instance ToHtml Link where
    toHtml l = withTagAttrs "a" [("href", href l)] $ toHtml (text l)

instance ToHtml Attr where
    toHtml (Attr s t) = s ++ if (t /= "") then "=\"" ++ t ++ "\"" else ""

showInnerTag :: HtmlTag -> String
showInnerTag t = unwords (tagname t : (map toHtml $ attrs t))

showHtmlContent :: Either String Html -> String
showHtmlContent (Left s) = s
showHtmlContent (Right h) = toHtml h

instance ToHtml Html where
    toHtml (PairTag open content close) = "<" ++ showInnerTag open ++ ">" ++ concatMap showHtmlContent content ++ "</" ++ showInnerTag close ++ ">"
    toHtml (SingleTag tag) = "<" ++ showInnerTag tag ++ "/>"
