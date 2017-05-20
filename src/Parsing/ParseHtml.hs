module Parsing.ParseHtml (html) where

import Data.Char (toLower)
import Text.Parsec

import AST
import Parsing.ParseOptions (allowUnsafeTags)
import Parsing.State
import Parsing.Utils

htmlTag :: HtmlTagType -> Parser HtmlTag
htmlTag tagType = do
    if tagType == Close
        then try (string "</") <?> "\"</\" (closing html tag)"
        else string "<" <?> "\"<\" (html tag)"
    spaces
    tagname <- many1 (letter <?> "rest of tag name") <?> "html tag name"
    let lowerTagname = map toLower tagname
    state <- getState
    let allowUnsafe = allowUnsafeTags $ options state
    failWithIf "script tags are not currently supported" (not allowUnsafe && lowerTagname == "script")
    attrs <- many attr
    if tagType == SelfClosing
        then try (string "/>") <?> "closing \"/>\" (self-closing html tag)"
        else string ">" <?> "closing \">\" (html tag)"
    return $ HtmlTag {tagname=lowerTagname, attrs=attrs}

htmlContent :: Parser (Either String Html)
htmlContent = fmap Left (many1 $ noneOf "<") <|> fmap Right html

pairTag :: Parser Html
pairTag = do
    open <- htmlTag Open
    content <- option [] $ many $ try htmlContent
    close <- htmlTag Close
    if tagname open /= tagname close
        then fail $ "mismatched tags: '" ++ tagname open ++ "' and '" ++ tagname close ++ "'"
        else return $ PairTag open content

singleTag :: Parser Html
singleTag = fmap SingleTag $ htmlTag SelfClosing

-- TODO: consider eliminating arbitrary-length backtracking on [singleTag]
-- by limiting tag name length and checking against self-closing tag names.
html :: Parser Html
html = try singleTag <|> pairTag

attr :: Parser Attr
attr = do
    space >> spaces
    name <- many1 letter <?> "html attribute name"
    char '='
    val <- attrVal
    return $ Attr name val

attrVal :: Parser String
attrVal = betweenWithErrors "\"" "\"" "html attribute value" (many $ noneOf "\"")
