module Render where

import Control.Monad.State.Lazy
import Data.List
import qualified Data.Map as M

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
    toHtml r (AST bs) = fmap unlines $ mapM (toHtml r) bs

withTag :: String -> String -> String
withTag tag content = "<" ++ tag ++ ">" ++ content ++ "</" ++ tag ++ ">"

showAttrs :: [(String, String)] -> String
showAttrs attrs = unwords $ map (\(name, val) -> name ++ "=\"" ++ val ++ "\"") attrs

withTagAttrs :: String -> [(String, String)] -> String -> String
withTagAttrs tag attrs content = "<" ++ tag ++ " " ++ showAttrs attrs ++ ">" ++ content ++ "</" ++ tag ++ ">"

fancyUnlines :: [String] -> String
fancyUnlines = concat . intersperse "\n"

instance ToHtml Block where
    toHtml r (Paragraph ls) = fmap (withTag "p" . fancyUnlines) $ mapM (toHtml r) ls
    toHtml r (Header level text) = fmap (withTag ("h" ++ show level )) $ toHtml r text
    toHtml r (UnorderedList ls) = fmap (withTag "ul" . unlines) $ mapM (fmap (withTag "li") . toHtml r) ls
    toHtml r (BlockQuote ls) = fmap (withTag "blockquote" . fancyUnlines) $ mapM (toHtml r) ls
    toHtml _ (BlockCode s) = return $ withTag "pre" $ withTag "code" $ unlines s
    toHtml r (FootnoteDef identifier ls) = fmap (withTagAttrs "p" [("id", (footnotePrefix r) ++ "-footnote-" ++ identifier)] . fancyUnlines) $ mapM (toHtml r) ls
    toHtml r (BlockHtml h) = toHtml r h

instance ToHtml Line where
    toHtml r (Line is) = fmap concat $ mapM (toHtml r) is

instance ToHtml Inline where
    toHtml r (Italics i) = toHtml r i >>= (return . withTag "i")
    toHtml r (Bold i) = toHtml r i >>= (return . withTag "b")
    toHtml _ (Code s) = return $ withTag "code" s
    toHtml r (FootnoteRef identifier) = do
        fs <- gets footnotes
        let newId = M.size fs
        put $ RenderState {footnotes=M.insert identifier newId fs}
        return $ withTag "sup" $ withTagAttrs "a" [("href", "#" ++ (footnotePrefix r) ++ "-footnote-" ++ identifier)] ("[" ++ show newId ++ "]")
    toHtml _ (Plaintext s) = return s
    toHtml r (InlineHtml h) = toHtml r h
    toHtml r (InlineLink l) = toHtml r l

instance ToHtml Link where
    toHtml r l = fmap (withTagAttrs "a" [("href", href l)] . concat) $ mapM (toHtml r) $ text l

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
