module Parsing.ParseInline where

import Data.Maybe
import Text.Parsec
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
    let maybeIndex = M.lookup identifier f
    if isJust maybeIndex
        then fail $ "repeated footnote identifier: " ++ (show $ fromJust maybeIndex)
        else return ()
    let index = M.size f
    let f' = M.insert identifier index f
    putState $ state {footnoteIndices=f'}
    return $ FootnoteRef index

link :: Parser Inline
link = do
    state <- getState
    let currentParserStack = inlineParserStack state
    lookAhead $ char '['
    if elem "link" currentParserStack
        then fail "links cannot be nested"
        else return ()
    text <- betweenWithErrors' "[" "]" "link text" $ withModifiedState (many1 inline) $ \s -> s {inlineParserStack=("link" : currentParserStack)}
    href <- betweenWithErrors' "(" ")" "link href" $ many $ escapableNoneOf "()"
    return $ Link {text=text, href=href}

inline :: Parser Inline
inline = choice [nestedBold, bold, italics, code, footnoteRef, link, fmap InlineHtml html, fmap Plaintext plaintext]
