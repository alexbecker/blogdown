module Render where

import Control.Monad.State.Lazy
import Data.List
import qualified Data.Map as M

import AST

data RenderState = RenderState {
    footnotes :: M.Map String Int
}

class ToHtml a where
    toHtml :: a -> State RenderState String

render :: (ToHtml a) => a -> String
render a = fst $ runState (toHtml a) RenderState{footnotes = M.fromList []}

instance ToHtml AST where
    toHtml (AST bs) = fmap unlines $ mapM toHtml bs

withTag :: String -> String -> String
withTag tag content = "<" ++ tag ++ ">" ++ content ++ "</" ++ tag ++ ">"

showAttrs :: [(String, String)] -> String
showAttrs attrs = unwords $ map (\(name, val) -> name ++ "=\"" ++ val ++ "\"") attrs

withTagAttrs :: String -> [(String, String)] -> String -> String
withTagAttrs tag attrs content = "<" ++ tag ++ " " ++ showAttrs attrs ++ ">" ++ content ++ "</" ++ tag ++ ">"

fancyUnlines :: [String] -> String
fancyUnlines = concat . intersperse "\n"

instance ToHtml Block where
    toHtml (Paragraph ls) = fmap (withTag "p" . fancyUnlines) $ mapM toHtml ls
    toHtml (Header level text) = fmap (withTag ("h" ++ show level )) $ toHtml text
    toHtml (UnorderedList ls) = fmap (withTag "ul" . unlines) $ mapM (fmap (withTag "li") . toHtml) ls
    toHtml (BlockQuote ls) = fmap (withTag "blockquote" . fancyUnlines) $ mapM toHtml ls
    toHtml (BlockCode s) = return $ withTag "pre" $ withTag "code" $ unlines s
    toHtml (FootnoteDef identifier ls) = fmap (withTagAttrs "p" [("id", "footnote-" ++ identifier)] . fancyUnlines) $ mapM toHtml ls
    toHtml (BlockHtml h) = toHtml h

instance ToHtml Line where
    toHtml (Line is) = fmap concat $ mapM toHtml is

instance ToHtml Inline where
    toHtml (InlineLink l) = toHtml l
    toHtml (InlineNonLink s) = toHtml s

instance ToHtml LinkContents where
    toHtml (Italics s) = return $ withTag "i" s
    toHtml (Bold s) = return $ withTag "b" s
    toHtml (Code s) = return $ withTag "code" s
    toHtml (FootnoteRef identifier) = do
        fs <- gets footnotes
        let newId = M.size fs
        put $ RenderState {footnotes=M.insert identifier newId fs}
        return $ withTag "sup" $ withTagAttrs "a" [("href", "#footnote-" ++ identifier)] ("[" ++ show newId ++ "]")
    toHtml (Plaintext s) = return s
    toHtml (InlineHtml h) = toHtml h

instance ToHtml Link where
    toHtml l = fmap (withTagAttrs "a" [("href", href l)] . concat) $ mapM toHtml (text l)

showAttr :: Attr -> String
showAttr (Attr s t) = s ++ if (t /= "") then "=\"" ++ t ++ "\"" else ""

showInnerTag :: HtmlTag -> String
showInnerTag t = unwords (tagname t : (map showAttr $ attrs t))

showHtmlContent :: Either String Html -> String
showHtmlContent (Left s) = s
showHtmlContent (Right h) = showHtml h

showHtml :: Html -> String
showHtml (PairTag open content close) = "<" ++ showInnerTag open ++ ">" ++ concatMap showHtmlContent content ++ "</" ++ showInnerTag close ++ ">"
showHtml (SingleTag tag) = "<" ++ showInnerTag tag ++ "/>"

instance ToHtml Html where
    toHtml = return . showHtml
