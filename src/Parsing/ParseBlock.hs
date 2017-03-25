module Parsing.ParseBlock where

import Text.Parsec

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
