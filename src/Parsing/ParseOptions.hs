{-# LANGUAGE DeriveGeneric #-}

module Parsing.ParseOptions where

import qualified Data.Map.Strict as M
import GHC.Generics (Generic)

data ParseOptions = ParseOptions {
    allowUnsafeTags :: Bool
} deriving (Show, Generic)

defaultParseOptions = ParseOptions {
    allowUnsafeTags = False 
}

parseOptions :: M.Map String ([String] -> ParseOptions -> ParseOptions)
parseOptions = M.fromList [
    ("--allow-unsafe-tags", \_ -> \p -> p {allowUnsafeTags=True})]
