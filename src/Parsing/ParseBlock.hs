module Parsing.ParseBlock where

import Text.Parsec hiding (char', string')

import AST
import Parsing.State
import Parsing.TextUtils
import Parsing.Utils
import Parsing.ParseHtml
import Parsing.ParseInline

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

listItem :: Bool -> Int -> Parser ListItem
listItem ordered depth = fmap ListItem $ do
    let identifier = replicate depth ' ' ++ if ordered
        then "- "
        else "* "
    let errMsg = "\"" ++ identifier ++ "\" " ++ if ordered
        then "(ordered list item)"
        else "(unordered list item)"
    try (string' identifier) <?> errMsg
    many1 inline

list :: Bool -> Int -> Parser List
list ordered depth = do
    first <- listItem ordered depth
    remainder <- many (listItem ordered depth
                   <|> fmap SubList (list True (depth + 1))
                   <|> fmap SubList (list False (depth + 1)))
    return $ List ordered $ first : remainder

listBlock :: Parser Block
listBlock = fmap ListBlock (list True 1 <|> list False 1)

blockQuotePrefix :: Parser String
blockQuotePrefix = try (string' "> ") <?> "\"> \" (blockquote)"

blockQuote :: Parser Block
blockQuote = fmap BlockQuote $ do
    blockQuotePrefix
    withModifiedState (many1 inline) $ \s -> s {skipPrefix=Just blockQuotePrefix}

blockCodeLineStart :: Parser String
blockCodeLineStart = try (string "\t" <|> string "    ") <?> "\"    \" or tab (code block)"

blockCodeIndented :: Parser Block
blockCodeIndented = fmap (BlockCode "" . unlines) $ many1 $ blockCodeLineStart >> manyTill (noneOf "\n") (char' '\n')

blockCodeFenced :: Parser Block
blockCodeFenced = do
    try $ string' "```" -- Backtrack to handle inline code at the start of a line.
    cls <- manyTill (noneOf "\n") (char' '\n')
    content <- many (escapableNoneOf "`")
    string' "```"
    return $ BlockCode cls content

blockHtml :: Parser Block
blockHtml = fmap BlockHtml html

-- Does not return TableRow because we don't know what type the cells are until the whole table is parsed.
tableRow :: Parser [[Inline]]
tableRow = manyTill ((char' '|' <?> "\"|\" (table cell)") >> many1 inline) (try $ string' "|\n")

tableSeparator :: Parser ()
tableSeparator = optional $ do
    sepBy1 (char '+' <?> "\"+\" (table)") $ many1 (char '-')
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
block = between
    (many $ char '\n')
    (many $ char '\n')
    (choice [blockHtml,
             hardRule,
             header,
             listBlock,
             blockQuote,
             table,
             blockCodeIndented,
             blockCodeFenced,
             paragraph])
