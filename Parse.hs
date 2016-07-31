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
    if tagname open /= tagname close
        then fail $ "mismatched tags: '" ++ tagname open ++ "' and '" ++ tagname close ++ "'"
        else return $ PairTag open content

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
attrVal = between (char '"') (char '"') (many $ noneOf "\"")

bold :: Parser Inline
bold = fmap Bold $ try $ between (string "**") (string "**") $ inlineExcept bold

italics :: Parser Inline
italics = fmap Italics $ between (char '*') (char '*') $ inlineExcept italics

code :: Parser Inline
code = fmap Code $ between (char '`') (char '`') $ many1 $ noneOf "`"

footnoteRef :: Parser Inline
footnoteRef = do
    char '^'
    identifier <- between (char '[') (char ']') $ many1 $ noneOf "[]"
    return $ FootnoteRef identifier

link :: Parser Link
link = do
    text <- between (char '[') (char ']') $ many1 $ inlineExcept inlineLink
    href <- between (char '(') (char ')') $ many ((string "\\)" >> return ')') <|> noneOf "\n)")
    return $ Link {text=text, href=href}

inlineLink :: Parser Inline
inlineLink = fmap InlineLink link

inline :: Parser Inline
inline = choice [bold,
                 italics,
                 code,
                 footnoteRef,
                 fmap InlineHtml html,
                 fmap InlineLink link,
                 fmap Plaintext (many1 $ noneOf ('\n' : specials))]

inlineExcept :: Parser Inline -> Parser Inline
inlineExcept p = do
    illegalParse <- optionMaybe p
    if isJust illegalParse
        then fail "cannot nest inline markup"
        else inline

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
    string " *"
    many1 $ oneOf " \t"
    line

blockQuote :: Parser Block
blockQuote = fmap BlockQuote $ flip sepEndBy1 (char '\n') $ try $ do
    char '>'
    many1 $ oneOf " \t"
    line

blockCode :: Parser Block
blockCode = fmap BlockCode $ flip sepEndBy1 (char '\n') $ try $ do
    char '\t' <|> (string "    " >> return '\t')
    many $ noneOf "\n"

footnoteDef :: Parser FootnoteDef
footnoteDef = do
    char '~'
    identifier <- between (char '[') (char ']') $ many1 $ noneOf "[]"
    many1 $ oneOf " \t"
    content <- many1 block
    return $ FootnoteDef identifier content

footnoteDefs :: Parser FootnoteDefs
footnoteDefs = fmap FootnoteDefs $ many1 footnoteDef

blockHtml :: Parser Block
blockHtml = fmap BlockHtml html

block :: Parser Block
block = (many $ char '\n') >> choice [blockHtml, paragraph, header, unorderedList, blockQuote, blockCode]

ast :: Parser AST
ast = do
    blocks <- many block
    footnotes <- optionMaybe footnoteDefs
    return $ AST blocks footnotes
