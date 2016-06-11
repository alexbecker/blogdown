module Parse where

import Data.Maybe
import Text.Parsec hiding (Line)
import Text.Parsec.Char

import AST

type Parser = Parsec String ()

nonSpecial :: Parser Char
nonSpecial = noneOf "*<>[]()"

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
bold = try $ fmap Bold $ between (string "**") (string "**") $ many1 nonSpecial

italics :: Parser Italics
italics = fmap Italics $ between (char '*') (char '*') $ many1 nonSpecial

link :: Parser Link
link = do
    text <- between (char '[') (char ']') line
    href <- between (char '(') (char ')') $ many ((string "\\)" >> return ')') <|> noneOf ")")
    return $ Link {text=text, href=href}

plaintext :: Parser String
plaintext = many1 $ noneOf "*<>[]()\r\n"

inline :: Parser Inline
inline = choice [fmap InlineBold bold,
                 fmap InlineItalics italics,
                 fmap InlineLink link,
                 fmap InlineHtml html,
                 fmap Plaintext plaintext]

line :: Parser Line
line = fmap Line (many1 inline)

paragraph :: Parser Block
paragraph = do
    ls <- sepEndBy1 line (char '\n' <|> (eof >> return '\n'))
    many $ char '\n'
    return $ Paragraph ls

block :: Parser Block
block = paragraph

ast :: Parser AST
ast = fmap AST (many block)
