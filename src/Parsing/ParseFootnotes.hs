module Parsing.ParseFootnotes where

import Data.Maybe
import Text.Parsec
import qualified Data.Map.Strict as M

import AST
import Parsing.State
import Parsing.TextUtils
import Parsing.ParseBlock

footnoteDef :: Parser FootnoteDef
footnoteDef = do
    many $ char '\n'
    identifier <- betweenWithErrors' "~[" "]" "footnote definition" $ many1 $ escapableNoneOf "[]"
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
