module Parsing.TextUtils where

import Data.Maybe
import Text.Parsec

import Parsing.State
import Parsing.Utils

-- Parse a single character and update parsing state accordingly.
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

-- Parse a string of characters and update parsing state accordingly.
string' :: String -> Parser String
string' = mapM char'

-- Parse a string of characters not in [blacklist], unless they are escaped,
-- and update state like [string'].
escapableNoneOf :: String -> Parser Char
escapableNoneOf blacklist = do
    escape <- suppressErr (optionMaybe $ char '\\')
    c <- if isJust escape
        then anyChar
        else noneOf blacklist
    modifyState $ \s -> s {prevCharIsNewline=(c == '\n')}
    return c

-- Like [betweenWithErrors], but updates state when parsing the start and end strings.
betweenWithErrors' :: String -> String -> String -> Parser a -> Parser a
betweenWithErrors' open close name = between
    (try (string' open) <?> "\"" ++ open ++ "\" (" ++ name ++ ")")
    (try (string' close) <?> "closing \"" ++ close ++ "\" (" ++ name ++ ")")
