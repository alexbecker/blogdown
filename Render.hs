module Render where

import Data.Maybe
import Text.Parsec.Char

import AST

class ToHtml a where
    toHtml :: a -> String

instance ToHtml Inline where
    toHtml (InlineItalics s) = toHtml s
    toHtml (InlineBold s) = toHtml s

instance ToHtml Italics where
    toHtml (Italics s) = "<i>" ++ s ++ "</i>"

instance ToHtml Bold where
    toHtml (Bold s) = "<b>" ++ s ++ "</b>"

showInnerTag :: HtmlTag -> String
showInnerTag t = unwords (tagname t : (map toHtml $ attrs t))

instance ToHtml Attr where
    toHtml (Attr s t) = s ++ if (t /= "") then "=\"" ++ t ++ "\"" else ""

showHtmlContent :: Either String Html -> String
showHtmlContent (Left s) = s
showHtmlContent (Right h) = toHtml h

instance ToHtml Html where
    toHtml (PairTag open content close) = "<" ++ showInnerTag open ++ ">" ++ concatMap showHtmlContent content ++ "</" ++ showInnerTag close ++ ">"
    toHtml (SingleTag tag) = "<" ++ showInnerTag tag ++ "/>"
