module Parsing.Parse where

import Text.Parsec hiding (parse)

import AST
import Parsing.ParseOptions (ParseOptions)
import Parsing.ParseBlock
import Parsing.ParseFootnotes
import Parsing.State

ast :: Parser AST
ast = do
    blocks <- many block
    footnotes <- optionMaybe footnoteDefs
    eof
    return $ AST blocks footnotes

parse :: ParseOptions -> String -> Either ParseError AST
parse parseOptions = runParser ast (initialState {options=parseOptions}) ""
