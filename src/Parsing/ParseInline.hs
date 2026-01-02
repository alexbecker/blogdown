module Parsing.ParseInline (inline) where

import Data.Maybe
import Network.URI (isURI)
import Text.Parsec hiding (char', string')
import qualified Data.Map.Strict as M

import AST
import Parsing.State
import Parsing.Text
import Parsing.TextUtils
import Parsing.Utils
import Parsing.ParseHtml

nestedBold :: Parser Inline
nestedBold = suppressErr (try (string "****")) >> fail "cannot have empty or nested bold nodes"

bold :: Parser Inline
bold = do
    state <- getState
    let currentParserStack = inlineParserStack state
    failIf $ elem "bold" currentParserStack
    s <- betweenWithErrors' "**" "**" "bold"
        $ withModifiedState (many1 inline) $ \s -> s {inlineParserStack=("bold" : currentParserStack)}
    return $ Bold s

-- The bold and italics parsers are tricky because they both use the same special character.
italics :: Parser Inline
italics = do
    state <- getState
    let currentParserStack = inlineParserStack state
    failIf $ elem "italics" currentParserStack
    s <- between
        (try ((char' '*' <?> "\"*\" (italics)") >> suppressErr (notFollowedBy (char '*'))))
        (char' '*' <?> "closing \"*\" (italics)")
        ((withModifiedState (many1 inline) $ \s -> s {inlineParserStack=("italics" : currentParserStack)}) <?>
            if elem "bold" currentParserStack
                then "content in italics node or extra \"*\" to close bold node"
                else "content in italics node")
    return $ Italics s

code :: Parser Inline
code = fmap Code $ betweenWithErrors' "`" "`" "code" $ many1 $ escapableNoneOf "`"

footnoteRef :: Parser Inline
footnoteRef = do
    identifier <- betweenWithErrors' "^[" "]" "footnote reference" $ many1 $ escapableNoneOf "[]"
    state <- getState
    let f = footnoteIndices state
    if M.member identifier f
        then fail $ "repeated footnote identifier: " ++ identifier
        else return ()
    let index = M.size f
    let f' = M.insert identifier index f
    putState $ state {footnoteIndices=f'}
    return $ FootnoteRef index

image :: Parser Inline
image = do
    alt <- betweenWithErrors' "![" "]" "image" $ many1 $ escapableNoneOf "[]"
    src <- betweenWithErrors' "(" ")" "image src" $ many $ escapableNoneOf "()"
    return $ Image {alt=alt, src=src}

link :: Parser Inline
link = do
    state <- getState
    let currentParserStack = inlineParserStack state
    lookAhead (char '[' <?> "\"[\" (link)")
    if elem "link" currentParserStack
        then fail "links cannot be nested"
        else return ()
    text <- betweenWithErrors' "[" "]" "link" $ withModifiedState (many1 inline) $ \s -> s {inlineParserStack=("link" : currentParserStack)}
    href <- optionMaybe $ betweenWithErrors' "(" ")" "link href" $ many $ escapableNoneOf "()"
    if isJust href
        then return $ Link {text=text, href=fromJust href}
        else do
            let text' = unboxPlaintext text where
                unboxPlaintext [Plaintext p] = p
                unboxPlaintext _ = ""
            if isURI text'
                then return $ Link {text=text, href=text'}
                else fail "link href is required unless link text is a valid absolute URI"

inline :: Parser Inline
inline = choice [nestedBold, bold, italics, code, footnoteRef, link, image, fmap InlineHtml html, fmap Plaintext plaintext]
