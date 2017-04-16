module Parsing.Text (plaintext) where

import Data.Maybe
import Text.Parsec

import Parsing.State
import Parsing.Utils

specials = "*`<>[]|\\"
specialStrings = ["^["]
firstCharSpecials = " \t#~+\n" ++ specials

-- Parse a single non-special character, allowing for escaping and continuation.
nonSpecial :: String -> Parser Char
nonSpecial blacklist = do
    escape <- suppressErr (optionMaybe $ char '\\')
    if isJust escape
        then do
            optional $ char '\n'    -- continuation
            anyChar
        else do
            maybeSpecialStrings <- mapM (\s -> suppressErr $ optionMaybe $ lookAhead $ try $ string s) specialStrings
            let specialStringsFound = catMaybes maybeSpecialStrings
            if length specialStringsFound > 0
                then fail $ "special string sequence \"" ++ head specialStringsFound ++ "\""
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
            c <- suppressErr (optionMaybe $ char '\n')
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
