module Parsing.State where

import Text.Parsec
import qualified Data.Map.Strict as M

import Parsing.ParseOptions

data ParserState = ParserState {
    prevCharIsNewline :: Bool,
    skipPrefix :: Maybe (Parser String),
    inlineParserStack :: [String],
    footnoteIndices :: M.Map String Int,
    options :: ParseOptions
}
type Parser = Parsec String ParserState

initialState = ParserState{
    prevCharIsNewline=False,
    skipPrefix=Nothing,
    inlineParserStack=[],
    footnoteIndices=M.fromList [],
    options=defaultParseOptions
}
