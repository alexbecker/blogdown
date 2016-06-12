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

bold :: Parser Bold
bold = try $ fmap Bold $ between (string "**") (string "**") $ many1 $ noneOf specials

italics :: Parser Italics
italics = fmap Italics $ between (char '*') (char '*') $ many1 $ noneOf specials

code :: Parser Code
code = fmap Code $ between (char '`') (char '`') $ many1 $ noneOf "`"

link :: Parser Link
link = do
    text <- between (char '[') (char ']') linkcontents
    href <- between (char '(') (char ')') $ many ((string "\\)" >> return ')') <|> noneOf (')' : specials))
    return $ Link {text=text, href=href}

linkcontents :: Parser LinkContents
linkcontents = choice [fmap LCBold bold,
                       fmap LCItalics italics,
                       fmap LCCode code,
                       fmap LCHtml html,
                       fmap Plaintext (many1 $ noneOf ('\n' : specials))]

footnoteRef :: Parser FootnoteRef
footnoteRef = do
    char '^'
    identifier <- between (char '[') (char ']') $ many1 alphaNum
    return $ FootnoteRef identifier

footnoteDef :: Parser Block
footnoteDef = do
    char '~'
    identifier <- between (char '[') (char ']') $ many1 alphaNum
    many $ oneOf " \t"
    content <- many1 line
    return $ FootnoteDef identifier content

inline :: Parser Inline
inline = fmap InlineLink link <|> fmap InlineNonLink linkcontents <|> fmap InlineFootnoteRef footnoteRef

line :: Parser Line
line = do
    startingSpace <- optionMaybe $ lookAhead $ oneOf " \t"
    if isJust startingSpace
        then fail "line cannot begin with a space or tab"
        else fmap Line $ many1 inline

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

unorderedList :: Parser Block
unorderedList = fmap UnorderedList $ flip sepEndBy1 (char '\n') $ try $ do
    many $ char ' '
    char '*'
    many $ oneOf " \t"
    line

blockQuote :: Parser Block
blockQuote = fmap BlockQuote $ flip sepEndBy1 (char '\n') $ try $ do
    many $ char ' '
    char '>'
    many $ oneOf " \t"
    line

blockCode :: Parser Block
blockCode = fmap BlockCode $ flip sepEndBy1 (char '\n') $ try $ do
    char '\t' <|> (string "    " >> return '\t')
    many $ oneOf " \t"
    many $ noneOf "\n"

block :: Parser Block
block = (many $ char '\n') >> choice [paragraph, header, unorderedList, blockQuote, blockCode, footnoteDef]

ast :: Parser AST
ast = fmap AST (many block)
