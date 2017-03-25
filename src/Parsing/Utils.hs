module Parsing.Utils where

import Text.Parsec

suppressErr :: Parsec s u a -> Parsec s u a
suppressErr p = p <?> ""

failWithIf :: String -> Bool -> Parsec s u ()
failWithIf msg cond = if cond then fail msg else return ()

failIf :: Bool -> Parsec s u ()
failIf = failWithIf ""

withModifiedState :: Parsec s u a -> (u -> u) -> Parsec s u a
withModifiedState p modifier = do
    state <- getState
    putState $ modifier state
    result <- p
    putState state
    return result

-- Like [between], but with more helpful error messages on failure.
betweenWithErrors :: String -> String -> String -> Parsec String u a -> Parsec String u a
betweenWithErrors open close name = between
    (try (string open) <?> "\"" ++ open ++ "\" (" ++ name ++ ")")
    (try (string close) <?> "closing \"" ++ close ++ "\" (" ++ name ++ ")")
