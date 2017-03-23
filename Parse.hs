module Parse where

import Data.List
import Data.Maybe
import Text.Parsec
import Text.Parsec.Char
import qualified Data.Map.Strict as M

import AST

data ParserState = ParserState {
    prevCharIsNewline :: Bool,
    skipPrefix :: Parser String,
    footnoteIndices :: M.Map String Int
}
type Parser = Parsec String ParserState

initialState = ParserState{
    prevCharIsNewline=False,
    skipPrefix=string "",
    footnoteIndices=M.fromList []
}

specials = "*`^<>[]|\\"
firstCharSpecials = " \t#~+\n" ++ specials

-- Parse a single non-special character, allowing for escaping and continuation.
nonSpecial :: String -> Parser Char
nonSpecial blacklist = do
    escape <- (optionMaybe $ char '\\') <?> ""  -- Suppress error message, since we never "expect" escape character.
    if isJust escape
        then do
            optional $ char '\n'    -- continuation
            anyChar
        else noneOf blacklist

-- Parse one or more non-special characters, allowing for escaping and incorporating the
-- rules for special characters at the start of a line. Note that this does not consume
-- as many non-special characters as possible, for ease of implementation.
nonSpecials :: Parser String
nonSpecials = do
    state <- getState
    str <- if prevCharIsNewline state
        then do
            s <- skipPrefix state
            if null s
                then do
                    c <- nonSpecial firstCharSpecials
                    return [c]
                else return s
        else do
            s <- many $ nonSpecial ('\n' : specials)
            c <- (optionMaybe $ char '\n') <?> "" -- Suppress error message, since we never "expect" newline in the middle of text.
            if isJust c
                then return (s ++ "\n")
                else if null s
                    then fail ""    -- Don't succeed without consuming any input.
                    else return s
    putState $ state {prevCharIsNewline=(last str == '\n')}
    return str

-- Parse as many non-special characters as possible.
plaintext :: Parser String
plaintext = fmap concat $ many1 nonSpecials

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

escapableNoneOf :: String -> Parser Char
escapableNoneOf blacklist = do
    escape <- (optionMaybe $ char '\\') <?> ""  -- Suppress error message, since we never "expect" escape character.
    c <- if isJust escape
        then anyChar
        else noneOf blacklist
    modifyState $ \s -> s {prevCharIsNewline=(c == '\n')}
    return c

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

-- Some inline parsers are not allowed to nest, and so we use a registry of all inline
-- parsers combined with the [internalParser] to nest the various internal parsers
-- appropriately.
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

-- Like [between], but with more helpful error messages on failure.
betweenWithErrors :: String -> String -> String -> Parser a -> Parser a
betweenWithErrors open close name = between
    (try (string' open) <?> "\"" ++ open ++ "\" (" ++ name ++ ")")
    (try (string' close) <?> "closing \"" ++ close ++ "\" (" ++ name ++ ")")

bold :: [String] -> Parser Inline
bold parserNames = fmap Bold $ betweenWithErrors "**" "**" "bold" $ internalParser "bold" parserNames

-- The bold and italics parsers are tricky because they both use the same special character.
italics :: [String] -> Parser Inline
italics parserNames = fmap Italics $ between
    ((try (char' '*' >> lookAhead (noneOf "*"))) <?> "\"*\" (italics)")
    (char' '*' <?> "closing \"*\" (italics)")
    $ internalParser "italics" parserNames

code :: Parser Inline
code = fmap Code $ betweenWithErrors "`" "`" "code" $ many1 $ escapableNoneOf "`"

footnoteRef :: Parser Inline
footnoteRef = do
    identifier <- betweenWithErrors "^[" "]" "footnote reference" $ many1 $ escapableNoneOf "[]"
    state <- getState
    let f = footnoteIndices state
    let maybeIndex = M.lookup identifier f
    if isJust maybeIndex
        then fail $ "repeated footnote identifier: " ++ (show $ fromJust maybeIndex)
        else return ()
    let index = M.size f
    let f' = M.insert identifier index f
    putState $ state {footnoteIndices=f'}
    return $ FootnoteRef index

link :: [String] -> Parser Inline
link parserNames = do
    text <- betweenWithErrors "[" "]" "link text" $ internalParser "link" parserNames
    href <- betweenWithErrors "(" ")" "link href" $ many $ escapableNoneOf "()"
    return $ Link {text=text, href=href}

inline :: Parser Inline
inline = choice $ inlineParsers ["bold", "italics", "code", "footnoteRef", "link", "inlineHtml", "plaintext"]

hardRule :: Parser Block
hardRule = do
    try (string "---") <?> "\"---\" (hard rule)"
    many (char '-')
    many1 (char '\n')
    return HardRule

paragraph :: Parser Block
paragraph = fmap Paragraph $ many1 inline

header :: Parser Block
header = do
    hashes <- (many1 $ char' '#') <?> "\"#\" (header)"
    many1 $ oneOf " \t"
    text <- many1 inline
    return $ Header (length hashes) text

listItem :: Bool -> Parser ListItem
listItem ordered = fmap (ListItem ordered) $ do
    let identifier = if ordered then " - " else " * "
    (try $ string' identifier) <?> ("\"" ++ identifier ++ "\" (list item)")
    many1 $ inline

orderedList :: Parser Block
orderedList = fmap OrderedList $ many1 $ listItem True

unorderedList :: Parser Block
unorderedList = fmap UnorderedList $ many1 $ listItem False

blockQuoteLineStart :: Parser String
blockQuoteLineStart = try (string "> ") <?> "\"> \" (blockquote)"

blockQuote :: Parser Block
blockQuote = fmap BlockQuote $ do
    blockQuoteLineStart
    withModifiedState (many1 inline) $ \s -> s {prevCharIsNewline=False, skipPrefix=(blockQuoteLineStart >> many (char ' '))}

blockCodeLineStart :: Parser String
blockCodeLineStart = try (string "\t" <|> string "    ") <?> "\"    \" or tab (code block)"

blockCode :: Parser Block
blockCode = fmap (BlockCode . unlines) $ many1 $ blockCodeLineStart >> manyTill (noneOf "\n") (char' '\n')

blockHtml :: Parser Block
blockHtml = fmap BlockHtml html

-- Does not return TableRow because we don't know what type the cells are until the whole table is parsed.
tableRow :: Parser [[Inline]]
tableRow = manyTill ((char' '|' <?> "\"|\" (table cell)") >> many1 inline) (try $ string' "|\n")

tableSeparator :: Parser ()
tableSeparator = optional $ do
    sepBy1 (char '+' <?> "\"+\" (table)") (optionMaybe (char ' ') >> many1 (char '-') >> optionMaybe (char ' '))
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
    identifier <- betweenWithErrors "~[" "]" "footnote definition" $ many1 $ escapableNoneOf "[]"
    state <- getState
    let maybeIndex = M.lookup identifier $ footnoteIndices state
    index <- if isNothing maybeIndex
        then fail $ "unreferenced footnote identifier: " ++ identifier
        else return $ fromJust maybeIndex
    many1 $ oneOf " \t"
    modifyState (\s -> s {prevCharIsNewline=False})
    content <- many1 $ try block
    return $ FootnoteDef index content

footnoteDefs :: Parser FootnoteDefs
footnoteDefs = fmap FootnoteDefs $ many1 footnoteDef

ast :: Parser AST
ast = do
    blocks <- many block
    footnotes <- optionMaybe footnoteDefs
    eof
    return $ AST blocks footnotes
