module Parse where

import Data.Maybe
import Text.Parsec hiding (Line)
import Text.Parsec.Char

import AST

type Parser = Parsec String ()

specials = "*#`^~<>[]"

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

bold :: Parser LinkContents
bold = fmap Bold $ try $ between (string "**") (string "**") $ many1 $ noneOf specials

italics :: Parser LinkContents
italics = fmap Italics $ between (char '*') (char '*') $ many1 $ noneOf specials

code :: Parser LinkContents
code = fmap Code $ between (char '`') (char '`') $ many1 $ noneOf "`"

footnoteRef :: Parser LinkContents
footnoteRef = do
    char '^'
    identifier <- between (char '[') (char ']') $ many1 alphaNum
    return $ FootnoteRef identifier

linkcontents :: Parser LinkContents
linkcontents = choice [bold,
                       italics,
                       code,
                       footnoteRef,
                       fmap InlineHtml html,
                       fmap Plaintext (many1 $ noneOf ('\n' : specials))]

link :: Parser Link
link = do
    text <- between (char '[') (char ']') $ many1 linkcontents
    href <- between (char '(') (char ')') $ many ((string "\\)" >> return ')') <|> noneOf (')' : specials))
    return $ Link {text=text, href=href}

inline :: Parser Inline
inline = fmap InlineLink link <|> fmap InlineNonLink linkcontents

line :: Parser Line
line = do
    startingSpace <- optionMaybe $ lookAhead $ oneOf " \t"
    if isJust startingSpace
        then fail "line cannot begin with a space or tab"
        else fmap Line $ many1 inline

lines1 :: Parser [Line]
lines1 = sepEndBy1 line (char '\n' <|> (eof >> return '\n'))

paragraph :: Parser Block
paragraph = do
    ls <- lines1
    many $ char '\n'
    return $ Paragraph ls

header :: Parser Block
header = do
    hashes <- many1 $ char '#'
    many1 $ oneOf " \t"
    text <- line
    many $ char '\n'
    return $ Header (length hashes) text

unorderedList :: Parser Block
unorderedList = fmap UnorderedList $ flip sepEndBy1 (char '\n') $ try $ do
    many1 $ oneOf " \t"
    char '*'
    many1 $ oneOf " \t"
    line

blockQuote :: Parser Block
blockQuote = fmap BlockQuote $ flip sepEndBy1 (char '\n') $ try $ do
    many $ oneOf " \t"
    char '>'
    many1 $ oneOf " \t"
    line

blockCode :: Parser Block
blockCode = fmap BlockCode $ flip sepEndBy1 (char '\n') $ try $ do
    char '\t' <|> (string "    " >> return '\t')
    many $ oneOf " \t"
    many $ noneOf "\n"

footnoteDef :: Parser Block
footnoteDef = do
    char '~'
    identifier <- between (char '[') (char ']') $ many1 alphaNum
    many1 $ oneOf " \t"
    content <- lines1
    return $ FootnoteDef identifier content

block :: Parser Block
block = (many $ char '\n') >> choice [fmap BlockHtml html, paragraph, header, unorderedList, blockQuote, blockCode, footnoteDef]

ast :: Parser AST
ast = fmap AST $ many block
