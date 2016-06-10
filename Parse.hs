module Parse where

import Text.Parsec hiding (Line)
import Text.Parsec.Char

import AST

type Parser = Parsec String ()

htmlTag :: HtmlTagType -> Parser HtmlTag
htmlTag tagType = do
    if tagType == Close then string "</" else string "<"
    spaces
    tagname <- many1 letter
    spaces
    attrs <- sepEndBy attr spaces
    if tagType == SelfClosing then string "/>" else string ">"
    return $ HtmlTag {tagname=tagname, attrs=attrs}

htmlContent :: Parser (Either String Html)
htmlContent = fmap Left (many1 $ noneOf "<") <|> fmap Right html

pairTag :: Parser Html
pairTag = do
    open <- htmlTag Open
    content <- option [] $ many $ try htmlContent
    close <- htmlTag Close
    return $ PairTag open content close

singleTag :: Parser Html
singleTag = fmap SingleTag $ htmlTag SelfClosing

html :: Parser Html
html = try pairTag <|> singleTag

attr :: Parser Attr
attr = do
    name <- many1 letter
    char '='
    val <- attrVal
    return $ Attr name val

attrVal :: Parser String
attrVal = between (char '"') (char '"') (many1 $ noneOf "\"")

italics' :: Parser String
italics' = between (char '*') (char '*') (many1 $ noneOf "*")

italics :: Parser Italics
italics = italics' >>= return . Italics

bold :: Parser Bold
bold = fmap Bold $ between (char '*') (char '*') italics'

inline :: Parser Inline
inline = choice [try (italics >>= return . InlineItalics),
                 bold >>= return . InlineBold]
