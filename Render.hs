module Render where

import Data.List
import Data.Maybe
import Text.Parsec.Char

import AST

class ToHtml a where
    toHtml :: a -> String

instance ToHtml AST where
    toHtml (AST bs) = unlines $ map toHtml bs

fancyUnlines :: [String] -> String
fancyUnlines = concat . intersperse "\n"

instance ToHtml Block where
    toHtml (Paragraph ls) = "<p>" ++ fancyUnlines (map toHtml ls) ++ "</p>"

instance ToHtml Line where
    toHtml (Line is) = concatMap toHtml is

instance ToHtml Inline where
    toHtml (InlineItalics i) = toHtml i
    toHtml (InlineBold b) = toHtml b
    toHtml (InlineLink l) = toHtml l
    toHtml (InlineHtml h) = toHtml h
    toHtml (Plaintext s) = s

instance ToHtml Italics where
    toHtml (Italics s) = "<i>" ++ s ++ "</i>"

instance ToHtml Bold where
    toHtml (Bold s) = "<b>" ++ s ++ "</b>"

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
