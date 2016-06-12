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

fancyUnlines :: [String] -> String
fancyUnlines = concat . intersperse "\n"

instance ToHtml Block where
    toHtml (Paragraph ls) = withTag "p" $ fancyUnlines $ map toHtml ls
    toHtml (Header level text) = withTag ("h" ++ show level ) $ toHtml text
    toHtml (UnorderedList ls) = withTag "ul" $ unlines $ map (withTag "li" . toHtml) ls
    toHtml (BlockQuote ls) = withTag "blockquote" $ fancyUnlines $ map toHtml ls
    toHtml (BlockCode s) = withTag "pre" $ withTag "code" $ unlines s
    toHtml (FootnoteDef identifier ls) = "<p id=\"footnote-" ++ identifier ++ "\">" ++ (fancyUnlines $ map toHtml ls) ++ "</p>"

instance ToHtml Line where
    toHtml (Line is) = concatMap toHtml is

instance ToHtml Inline where
    toHtml (InlineLink l) = toHtml l
    toHtml (InlineNonLink s) = toHtml s
    toHtml (InlineFootnoteRef f) = toHtml f

instance ToHtml FootnoteRef where
    toHtml (FootnoteRef identifier) = withTag "sup" ("<a href=\"" ++ "#footnote-" ++ identifier ++ "\">[" ++ "0" ++ "]</a>")

instance ToHtml LinkContents where
    toHtml (LCItalics i) = toHtml i
    toHtml (LCBold b) = toHtml b
    toHtml (LCCode c) = toHtml c
    toHtml (LCHtml h) = toHtml h
    toHtml (Plaintext s) = s

instance ToHtml Italics where
    toHtml (Italics s) = withTag "i" s

instance ToHtml Bold where
    toHtml (Bold s) = withTag "b" s

instance ToHtml Code where
    toHtml (Code s) = withTag "code" s

instance ToHtml Link where
    toHtml l = "<a href=\"" ++ href l ++ "\">" ++ toHtml (text l) ++ "</a>"

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
