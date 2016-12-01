module Parse where

import Data.List
import Data.Maybe
import Text.Parsec
import Text.Parsec.Char

import AST

data ParserState = ParserState {
    prevCharIsNewline :: Bool,
    skipPrefix :: Parser String
}
type Parser = Parsec String ParserState

initialState = ParserState{
    prevCharIsNewline=False,
    skipPrefix=string ""
}

specials = "*`^<>[]|\\"
firstCharSpecials = " \t#~+\n" ++ specials

nonSpecial :: Parser Char
nonSpecial = do
    state <- getState
    escape <- optionMaybe $ char '\\'
    c <- if isJust escape
        then anyChar
        else if prevCharIsNewline state
            then skipPrefix state >> noneOf firstCharSpecials
            else noneOf specials
    putState $ state {prevCharIsNewline=(c == '\n')}
    return c

char' :: Char -> Parser Char
char' c = if c == '\n'
    then do
        newline <|> (eof >> return '\n')
        modifyState $ \s -> s {prevCharIsNewline=True}
        return '\n'
    else do
        char c
        modifyState $ \s -> s {prevCharIsNewline=False}
        return c

string' :: String -> Parser String
string' = mapM char'

withModifiedState :: Parser a -> (ParserState -> ParserState) -> Parser a
withModifiedState p modifier = do
    state <- getState
    putState $ modifier state
    result <- p
    putState state
    return result

plaintext :: Parser String
plaintext = many1 nonSpecial

escapableNoneOf :: String -> Parser Char
escapableNoneOf blacklist = do
    escape <- optionMaybe $ char '\\'
    c <- if isJust escape
        then anyChar
        else noneOf blacklist
    modifyState $ \s -> s {prevCharIsNewline=(c == '\n')}
    return c

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

inlineParsers :: [String] -> [Parser Inline]
inlineParsers parserNames = map snd $ filter (\(k,v) -> elem k parserNames)
    [("bold", bold parserNames),
     ("italics", italics parserNames),
     ("code", code),
     ("footnoteRef", footnoteRef),
     ("link", link parserNames),
     ("inlineHtml", fmap InlineHtml html),
     ("plaintext", fmap Plaintext plaintext)]

internalParser :: String -> [String] -> Parser [Inline]
internalParser parentName parserNames = many1 $ choice $ inlineParsers $ delete parentName parserNames

-- The bold and italics parsers are tricky because they both use the same special character.
-- As a result, both need "try" so they do not step on each other.
bold :: [String] -> Parser Inline
bold parserNames = fmap Bold $ try $ between (string' "**") (try $ string' "**") $ internalParser "bold" parserNames

italics :: [String] -> Parser Inline
italics parserNames = fmap Italics $ try $ between (char' '*') (char' '*') $ internalParser "italics" parserNames

code :: Parser Inline
code = fmap Code $ between (char' '`') (char' '`') $ many1 $ escapableNoneOf "`"

footnoteRef :: Parser Inline
footnoteRef = do
    char '^'
    identifier <- between (char' '[') (char' ']') $ many1 $ escapableNoneOf "[]"
    return $ FootnoteRef identifier

link :: [String] -> Parser Inline
link parserNames = do
    text <- between (char' '[') (char' ']') $ internalParser "link" parserNames
    href <- between (char' '(') (char' ')') $ many $ escapableNoneOf "()"
    return $ Link {text=text, href=href}

inline :: Parser Inline
inline = choice $ inlineParsers ["bold", "italics", "code", "footnoteRef", "link", "inlineHtml", "plaintext"]

hardRule :: Parser Block
hardRule = try (string "---") >> many (char '-') >> many1 (char '\n') >> return HardRule

paragraph :: Parser Block
paragraph = fmap Paragraph $ many1 inline

header :: Parser Block
header = do
    hashes <- many1 $ char' '#'
    many1 $ oneOf " \t"
    text <- many1 inline
    return $ Header (length hashes) text

listItem :: Bool -> Parser ListItem
listItem ordered = fmap (ListItem ordered) $ do
    let identifier = if ordered then " -" else " *"
    try $ string' identifier
    many1 $ oneOf " \t"
    many1 $ inline

orderedList :: Parser Block
orderedList = fmap OrderedList $ many1 $ listItem True

unorderedList :: Parser Block
unorderedList = fmap UnorderedList $ many1 $ listItem False

blockQuote :: Parser Block
blockQuote = fmap BlockQuote $ do
    string "> "
    withModifiedState (many1 inline) $ \s -> s {prevCharIsNewline=False, skipPrefix=string "> "}

blockCode :: Parser Block
blockCode = fmap (BlockCode . unlines) $ many1 $ (string "\t" <|> string "    ") >> manyTill (noneOf "\n") (char' '\n')

blockHtml :: Parser Block
blockHtml = fmap BlockHtml html

tableCellSeparator :: Parser Char
tableCellSeparator = try $ do
    char' '|'
    lookAhead $ noneOf "\n"

-- Does not return TableRow because we don't know what type the cells are until the whole table is parsed.
tableRow :: Parser [[Inline]]
tableRow = manyTill (char' '|' >> many1 inline) (try $ string' "|\n")

tableSeparator :: Parser ()
tableSeparator = optional $ do
    sepBy1 (char '+') (optionMaybe (char ' ') >> many1 (char '-') >> optionMaybe (char ' '))
    char' '\n'

table :: Parser Block
table = do
    tableSeparator
    rows <- many1 $ tableRow
    tableSeparator
    rows2 <- many $ tableRow
    let headerRows = if null rows2
        then Nothing
        else Just $ map (TableRow . map TableHeaderCell) rows
    let bodyRows = map (TableRow . map TableBodyCell) $ if null rows2 then rows else rows2
    if null rows2
        then return ()
        else tableSeparator
    return $ Table headerRows bodyRows

block :: Parser Block
block = (many $ char '\n') >> choice [blockHtml, hardRule, header, orderedList, unorderedList, blockQuote, table, blockCode, paragraph]

footnoteDef :: Parser FootnoteDef
footnoteDef = do
    many $ char '\n'
    char '~'
    identifier <- between (char' '[') (char' ']') $ many1 $ escapableNoneOf "[]"
    many1 $ oneOf " \t"
    modifyState (\s -> s {prevCharIsNewline=False})
    content <- many1 $ try block
    return $ FootnoteDef identifier content

footnoteDefs :: Parser FootnoteDefs
footnoteDefs = fmap FootnoteDefs $ many1 footnoteDef

ast :: Parser AST
ast = do
    blocks <- many block
    footnotes <- optionMaybe footnoteDefs
    return $ AST blocks footnotes
