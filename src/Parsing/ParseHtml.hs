module Parsing.ParseHtml (html) where

import Text.Parsec

import AST
import Parsing.State
import Parsing.Utils

htmlTag :: HtmlTagType -> Parser HtmlTag
htmlTag tagType = do
    if tagType == Close
        then try (string "</") <?> "\"</\" (closing html tag)"
        else string "<" <?> "\"<\" (html tag)"
    spaces
    tagname <- many1 letter <?> "html tag name"
    spaces
    attrs <- sepEndBy attr spaces
    if tagType == SelfClosing
        then try (string "/>") <?> "closing \"/>\" (self-closing html tag)"
        else string ">" <?> "closing \">\" (html tag)"
    return $ HtmlTag {tagname=tagname, attrs=attrs}

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
    name <- many1 letter <?> "html attribute name"
    char '='
    val <- attrVal
    return $ Attr name val

attrVal :: Parser String
attrVal = betweenWithErrors "\"" "\"" "html attribute value" (many $ noneOf "\"")
