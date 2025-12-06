module Render where

import Control.Monad.State.Lazy
import Data.List
import Data.List.Utils
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import AST
import RenderOptions

class ToHtml a where
    toHtml :: RenderOptions -> a -> String

instance ToHtml AST where
    toHtml r (AST bs Nothing) = unlines $ map (toHtml r) bs
    toHtml r (AST bs (Just f)) = body ++ footnotes ++ "\n" where
        body = unlines $ map (toHtml r) bs
        footnotes = toHtml r f

withTag :: String -> String -> String
withTag tag content = "<" ++ tag ++ ">" ++ content ++ "</" ++ tag ++ ">"

showAttrs :: [(String, String)] -> String
showAttrs attrs = unwords $ map (\(name, val) -> name ++ "=\"" ++ val ++ "\"") attrs

withTagAttrs :: String -> [(String, String)] -> String -> String
withTagAttrs tag attrs content = "<" ++ tag ++ " " ++ showAttrs attrs ++ ">" ++ content ++ "</" ++ tag ++ ">"

fancyUnlines :: [String] -> String
fancyUnlines = concat . intersperse "\n"

escapeHtml :: String -> String
escapeHtml = concatMap escapeChar where
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar c = [c]

instance ToHtml FootnoteDefs where
    toHtml r (FootnoteDefs fs) = withTagAttrs "ol"
        [("start", show $ footnoteIndexFrom r), ("class", "footnotes")]
        $ unlines $ map (toHtml r) $ sortOn index fs

instance ToHtml FootnoteDef where
    toHtml r (FootnoteDef index ls) = withTagAttrs "li" [("id", (footnotePrefix r) ++ "-footnote-" ++ show index)] content' where
        content = fancyUnlines $ map (toHtml r) ls
        content' = if footnoteBacklinks r
            then withTagAttrs "a" [("href", "#a-" ++ (footnotePrefix r) ++ "-footnote-" ++ show index)] "^" ++ "\n" ++ content
            else content

stripEndingNewline :: String -> String
stripEndingNewline s = if last s == '\n'
    then init s
    else s

instance ToHtml Block where
    toHtml _ HardRule = "<hr/>"
    toHtml r (Paragraph ls) = withTag "p" $ stripEndingNewline $ concatMap (toHtml r) ls
    toHtml r (Header level text) = withTag ("h" ++ show level) $ stripEndingNewline $ concatMap (toHtml r) text
    toHtml r (OrderedList ls) = withTag "ol" $ unlines $ map (toHtml r) ls
    toHtml r (UnorderedList ls) = withTag "ul" $ unlines $ map (toHtml r) ls
    toHtml r (BlockQuote ls) = withTag "blockquote" $ stripEndingNewline $ concatMap (toHtml r) ls
    toHtml _ (BlockCode s) = withTag "pre" $ withTag "code" $ escapeHtml s
    toHtml r (BlockHtml h) = toHtml r h
    toHtml r (Table Nothing trs) = withTag "table" $ withTag "tbody" body where
        body = unlines $ map (toHtml r) trs
    toHtml r (Table (Just ths) trs) = withTag "table" $ (withTag "thead" header) ++ (withTag "tbody" body) where
        header = unlines $ map (toHtml r) ths
        body = unlines $ map (toHtml r) trs

instance ToHtml ListItem where
    toHtml r (ListItem _ ls) = withTag "li" $ stripEndingNewline $ concatMap (toHtml r) ls

instance ToHtml TableCell where
    toHtml r (TableHeaderCell tds) = withTag "th" $ concatMap (toHtml r) tds
    toHtml r (TableBodyCell tds) = withTag "td" $ concatMap (toHtml r) tds

instance ToHtml TableRow where
    toHtml r (TableRow tcs) = withTag "tr" $ unlines $ map (toHtml r) tcs

instance ToHtml Inline where
    toHtml r (Italics ls) = withTag "i" $ concatMap (toHtml r) ls
    toHtml r (Bold ls) = withTag "b" $ concatMap (toHtml r) ls
    toHtml _ (Code s) = withTag "code" $ escapeHtml s
    toHtml r (FootnoteRef index) = withTag "sup" $ withTagAttrs "a"
        [("href", "#" ++ (footnotePrefix r) ++ "-footnote-" ++ show index),
         ("id", "a-" ++ (footnotePrefix r) ++ "-footnote-" ++ show index)]
        ("[" ++ show index ++ "]")
    toHtml r (Plaintext s) = escapeHtml $ if emDashes r
        then replace "--" "&mdash;" s
        else s
    toHtml r (InlineHtml h) = toHtml r h
    toHtml r (Link text href) = withTagAttrs "a" [("href", href)] $ concatMap (toHtml r) text

showAttr :: Attr -> String
showAttr (Attr s t) = s ++ "=\"" ++ t ++ "\""

showInnerTag :: HtmlTag -> String
showInnerTag t = unwords (tagname t : (map showAttr $ attrs t))

showHtmlContent :: Either String Html -> String
showHtmlContent (Left s) = s
showHtmlContent (Right h) = showHtml h

showHtml :: Html -> String
showHtml (PairTag open content) = "<" ++ showInnerTag open ++ ">" ++ concatMap showHtmlContent content ++ "</" ++ tagname open ++ ">"
showHtml (SingleTag tag) = "<" ++ showInnerTag tag ++ "/>"

instance ToHtml Html where
    toHtml _ = showHtml
