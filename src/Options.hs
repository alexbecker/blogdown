module Options where

import Data.Either
import Data.List
import qualified Data.Map.Strict as M
import System.Environment (getArgs)

import Parsing.ParseOptions
import Rendering.RenderOptions

isNotFlag :: String -> Bool
isNotFlag s = not $ isPrefixOf "--" s

splitArgsByFlag :: [String] -> [[String]]
splitArgsByFlag [] = []
splitArgsByFlag (flag : args) = (flag : (takeWhile isNotFlag args)) : (splitArgsByFlag $ dropWhile isNotFlag args)

option :: [String] -> Either (ParseOptions -> ParseOptions) (RenderOptions -> RenderOptions)
option (flag: flagArgs) = if M.member flag parseOptions
    then Left $ (parseOptions M.! flag) flagArgs
    else Right $ (renderOptions M.! flag) flagArgs

options :: [String] -> (ParseOptions, RenderOptions)
options args = (foldl (.) id parseOptionsMods defaultParseOptions,
                foldl (.) id renderOptionsMods defaultRenderOptions)
    where
        eitherMods = map option $ splitArgsByFlag args
        (parseOptionsMods, renderOptionsMods) = partitionEithers eitherMods

getOptions :: IO (ParseOptions, RenderOptions)
getOptions = fmap options getArgs
