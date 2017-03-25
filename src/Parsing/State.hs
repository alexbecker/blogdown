module Parsing.State where

import Text.Parsec
import qualified Data.Map.Strict as M

data ParserState = ParserState {
    prevCharIsNewline :: Bool,
    skipPrefix :: Parser String,
    inlineParserStack :: [String],
    footnoteIndices :: M.Map String Int
}
type Parser = Parsec String ParserState

initialState = ParserState{
    prevCharIsNewline=False,
    skipPrefix=string "",
    inlineParserStack=[],
    footnoteIndices=M.fromList []
}
