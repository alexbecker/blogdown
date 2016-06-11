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

bold :: Parser Bold
bold = fmap Bold $ between (string "**") (string "**") (many1 $ noneOf "<>*")

italics :: Parser Italics
italics = fmap Italics $ between (char '*') (char '*') (many1 $ noneOf "<>*")

inline :: Parser Inline
inline = fmap InlineBold bold <|> fmap InlineItalics italics <|> fmap InlineHtml html <|> fmap Plaintext (many1 $ noneOf "<>*")

line :: Parser Line
line = fmap Line (many1 inline)

paragraph :: Parser Paragraph
paragraph = fmap Paragraph (sepEndBy1 line $ char '\n')

block :: Parser Block
block = do
    p <- paragraph
    char '\n' <|> (eof >> return '\n')
    return $ ParagraphBlock p

ast :: Parser AST
ast = fmap AST (many block)
