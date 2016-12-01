module Render where

import Control.Monad.State.Lazy
import Data.List
import Data.List.Utils
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import AST
import RenderOptions

data RenderState = RenderState {
    footnotes :: M.Map String Int
}

initialState = RenderState{footnotes = M.fromList []}

class ToHtml a where
    toHtml :: RenderOptions -> a -> State RenderState String

render :: (ToHtml a) => RenderOptions -> a -> String
render r a = fst $ runState (toHtml r a) initialState

instance ToHtml AST where
    toHtml r (AST bs Nothing) = fmap unlines $ mapM (toHtml r) bs
    toHtml r (AST bs (Just f)) = do
        blocks <- mapM (toHtml r) bs
        footnotes <- toHtml r f
        return $ unlines blocks ++ footnotes ++ "\n"

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
    toHtml r (FootnoteDefs fs) = do
        fsRendered <- mapM (toHtml r) fs
        mapping <- gets footnotes
        let permutation = sortOn (fromJust . flip M.lookup mapping . identifier . (fs !!)) [0 .. length fs - 1]
        let fsSorted = map (fsRendered !!) permutation
        return $ (withTagAttrs "ol" [("start", show $ footnoteIndexFrom r), ("class", "footnotes")] . unlines) fsSorted

instance ToHtml FootnoteDef where
    toHtml r (FootnoteDef identifier ls) = do
    content <- mapM (toHtml r) ls
    let content' = if footnoteBacklinks r
        then withTagAttrs "a" [("href", "#a-" ++ (footnotePrefix r) ++ "-footnote-" ++ identifier)] "^" ++ "\n" ++ fancyUnlines content
        else fancyUnlines content
    return $ withTagAttrs "li" [("id", (footnotePrefix r) ++ "-footnote-" ++ identifier)] content'

stripEndingNewline :: String -> String
stripEndingNewline s = if last s == '\n'
    then init s
    else s

instance ToHtml Block where
    toHtml _ HardRule = return "<hr/>"
    toHtml r (Paragraph ls) = fmap (withTag "p" . stripEndingNewline . concat) $ mapM (toHtml r) ls
    toHtml r (Header level text) = fmap (withTag ("h" ++ show level) . stripEndingNewline . concat) $ mapM (toHtml r) text
    toHtml r (OrderedList ls) = fmap (withTag "ol" . unlines) $ mapM (toHtml r) ls
    toHtml r (UnorderedList ls) = fmap (withTag "ul" . unlines) $ mapM (toHtml r) ls
    toHtml r (BlockQuote ls) = fmap (withTag "blockquote" . stripEndingNewline . concat) $ mapM (toHtml r) ls
    toHtml _ (BlockCode s) = return $ withTag "pre" $ withTag "code" $ escapeHtml s
    toHtml r (BlockHtml h) = toHtml r h
    toHtml r (Table Nothing trs) = do
        bodyRows <- mapM (toHtml r) trs
        return $ withTag "table" $ withTag "tbody" $ unlines bodyRows
    toHtml r (Table (Just ths) trs) = do
        headerRows <- mapM (toHtml r) ths
        bodyRows <- mapM (toHtml r) trs
        return $ withTag "table" $ (withTag "thead" $ unlines headerRows) ++ (withTag "tbody" $ unlines bodyRows)

instance ToHtml ListItem where
    toHtml r (ListItem _ ls) = fmap (withTag "li" . stripEndingNewline . concat) $ mapM (toHtml r) ls

instance ToHtml TableCell where
    toHtml r (TableHeaderCell tds) = fmap (withTag "th" . concat) $ mapM (toHtml r) tds
    toHtml r (TableBodyCell tds) = fmap (withTag "td" . concat) $ mapM (toHtml r) tds

instance ToHtml TableRow where
    toHtml r (TableRow tcs) = fmap (withTag "tr" . unlines) $ mapM (toHtml r) tcs

instance ToHtml Inline where
    toHtml r (Italics ls) = fmap (withTag "i" . concat) $ mapM (toHtml r) ls
    toHtml r (Bold ls) = fmap (withTag "b" . concat) $ mapM (toHtml r) ls
    toHtml _ (Code s) = return $ withTag "code" $ escapeHtml s
    toHtml r (FootnoteRef identifier) = do
        fs <- gets footnotes
        let newId = M.size fs
        if isJust (M.lookup identifier fs)
            then fail $ "repeated footnote identifier: " ++ identifier
            else do
                put $ RenderState {footnotes=M.insert identifier newId fs}
                return $ withTag "sup" $ withTagAttrs "a"
                    [("href", "#" ++ (footnotePrefix r) ++ "-footnote-" ++ identifier),
                     ("id", "a-" ++ (footnotePrefix r) ++ "-footnote-" ++ identifier)]
                    ("[" ++ show newId ++ "]")
    toHtml r (Plaintext s) = if emDashes r
        then return $ replace "--" "&mdash;" s
        else return s
    toHtml r (InlineHtml h) = toHtml r h
    toHtml r (Link text href) = fmap (withTagAttrs "a" [("href", href)] . concat) $ mapM (toHtml r) $ text

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
    toHtml _ = return . showHtml
