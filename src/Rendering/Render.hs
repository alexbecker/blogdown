module Rendering.Render (ToHtml, toHtml) where

import Data.List
import Data.List.Utils
import Data.Maybe (isJust, fromJust)

import AST
import qualified Footnotes_js as JS
import qualified Footnotes_css as CSS
import Rendering.RenderOptions

class ToHtml a where
    toHtml :: RenderOptions -> a -> String

optionalJS :: RenderOptions -> String
optionalJS r = if (inlineJS r)
    then "<script>" ++ JS.content ++ "</script>\n"
    else ""

optionalCSS :: RenderOptions -> String
optionalCSS r = if (inlineCSS r)
    then "<style>" ++ CSS.content ++ "</style>\n"
    else ""

instance ToHtml AST where
    toHtml r (AST bs Nothing) = (unlines $ map (toHtml r) bs) ++ optionalCSS r ++ optionalJS r
    toHtml r (AST bs (Just f)) = body ++ footnotes ++ "\n" ++ optionalCSS r ++ optionalJS r where
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
    toHtml r (ListBlock l) = toHtml r l
    toHtml r (BlockQuote ls) = withTag "blockquote" $ stripEndingNewline $ concatMap (toHtml r) ls
    toHtml _ (BlockCode s) = withTag "pre" $ withTag "code" $ escapeHtml s
    toHtml r (BlockHtml h) = toHtml r h
    toHtml r (Table Nothing trs) = withTag "table" $ withTag "tbody" body where
        body = unlines $ map (toHtml r) trs
    toHtml r (Table (Just ths) trs) = withTag "table" $ (withTag "thead" header) ++ (withTag "tbody" body) where
        header = unlines $ map (toHtml r) ths
        body = unlines $ map (toHtml r) trs

instance ToHtml List where
    toHtml r (List True ls) = withTag "ol" $ unlines $ map (toHtml r) ls
    toHtml r (List False ls) = withTag "ul" $ unlines $ map (toHtml r) ls

instance ToHtml ListItem where
    toHtml r (ListItem ls) = withTag "li" $ stripEndingNewline $ concatMap (toHtml r) ls
    toHtml r (SubList l) = toHtml r l

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
        ("[" ++ show (index + footnoteIndexFrom r) ++ "]")
    toHtml r (Plaintext s) = escapeHtml $ if emDashes r
        then replace "--" "&mdash;" s
        else s
    toHtml r (InlineHtml h) = toHtml r h
    toHtml r (Link text href) = withTagAttrs "a" [("href", href)] $ concatMap (toHtml r) text
    toHtml _ (Image alt src) = "<img alt=\"" ++ alt ++ "\" src=\"" ++ src ++ "\"/>"

instance ToHtml HtmlTag where
    toHtml r (HtmlTag name attrs) = unwords (name : (map showAttr filteredAttrs)) where
        showAttr (Attr s t) = s ++ "=\"" ++ t ++ "\""
        filteredAttrs = maybe attrs (\allowed -> filter (\(Attr name val) -> elem name allowed) attrs) $ allowedAttributes r

showHtmlContent :: RenderOptions -> Either String Html -> String
showHtmlContent _ (Left s) = s
showHtmlContent r (Right h) = toHtml r h

instance ToHtml Html where
    toHtml r (PairTag open content) = concat [
        leftBracket,
        toHtml r open,
        rightBracket,
        concatMap (showHtmlContent r) content,
        leftBracket ++ "/",
        tagname open,
        rightBracket]
        where
            allowTag = maybe True (\attrs -> elem (tagname open) attrs) $ allowedTags r
            leftBracket = if allowTag then "<" else "&lt;"
            rightBracket = if allowTag then ">" else "&gt;"
    toHtml r (SingleTag tag) = concat [
        leftBracket,
        toHtml r tag,
        "/" ++ rightBracket]
        where
            allowTag = maybe True (\attrs -> elem (tagname tag) attrs) $ allowedTags r
            leftBracket = if allowTag then "<" else "&lt;"
            rightBracket = if allowTag then ">" else "&gt;"
