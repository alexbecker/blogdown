module Parse where

import Data.Maybe
import Text.Parsec hiding (Line)
import Text.Parsec.Char

import AST

type Parser = Parsec String ()

specials = "*#`<>[]()"

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
bold = try $ fmap Bold $ between (string "**") (string "**") $ many1 $ noneOf specials

italics :: Parser Italics
italics = fmap Italics $ between (char '*') (char '*') $ many1 $ noneOf specials

code :: Parser Code
code = fmap Code $ between (char '`') (char '`') $ many1 $ noneOf "`"

link :: Parser Link
link = do
    text <- between (char '[') (char ']') linkcontents
    href <- between (char '(') (char ')') $ many ((string "\\)" >> return ')') <|> noneOf ")")
    return $ Link {text=text, href=href}

linkcontents :: Parser LinkContents
linkcontents = choice [fmap InlineBold bold,
                       fmap InlineItalics italics,
                       fmap InlineCode code,
                       fmap InlineHtml html,
                       fmap Plaintext (many1 $ noneOf ('\n' : specials))]

inline :: Parser Inline
inline = fmap InlineLink link <|> fmap InlineNonLink linkcontents

line :: Parser Line
line = fmap Line (many1 inline)

paragraph :: Parser Block
paragraph = do
    ls <- sepEndBy1 line (char '\n' <|> (eof >> return '\n'))
    many $ char '\n'
    return $ Paragraph ls

header :: Parser Block
header = do
    hashes <- many1 $ char '#'
    many1 $ char ' '
    text <- line
    many $ char '\n'
    return $ Header (length hashes) text

block :: Parser Block
block = paragraph <|> header

ast :: Parser AST
ast = fmap AST (many block)
