import Data.List
import Data.List.Utils (replace)
import Data.Maybe (fromJust)
import Distribution.Simple
import Distribution.Simple.Setup

main = defaultMainWithHooks simpleUserHooks { preBuild=preBuild' }

preBuild' a b = compileStaticFiles >> preBuild simpleUserHooks a b

compileStaticFiles :: IO ()
compileStaticFiles = mapM_ compileStaticFile [
    "assets/Footnotes.js",
    "assets/Footnotes.css"]

compileStaticFile :: FilePath -> IO ()
compileStaticFile filepath = do
    let out = replace "." "_" filepath ++ ".hs"
    let moduleName = takeWhile (/= '.') $ fromJust $ stripPrefix "assets/" out
    rawContent <- readFile filepath
    let escaped = (replace "\"" "\\\"" $ intercalate "\\n" $ lines rawContent) ++ "\\n"
    let variable = "content = \"" ++ escaped ++ "\""
    let hsContent = "module " ++ moduleName ++ " where\n" ++ variable
    writeFile out hsContent
