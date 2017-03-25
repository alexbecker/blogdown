module Parsing.Parse where

import Text.Parsec

import AST
import Parsing.State
import Parsing.ParseBlock
import Parsing.ParseFootnotes

ast :: Parser AST
ast = do
    blocks <- many block
    footnotes <- optionMaybe footnoteDefs
    eof
    return $ AST blocks footnotes
