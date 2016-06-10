module Main where

import Data.Maybe
import Text.Parsec hiding (Line)
import Text.Parsec.Char

type Parser = Parsec String ()

data AST = AST [Block]

data Block = ParagraphBlock Paragraph

data Paragraph = Paragraph [Line]

data Line = Line [Inline]

data Inline = InlineItalics Italics
            | InlineBold Bold
instance Show Inline where
    show (InlineItalics s) = show s
    show (InlineBold s) = show s

data Italics = Italics String
instance Show Italics where
    show (Italics s) = "<i>" ++ s ++ "</i>"

data Bold = Bold String
instance Show Bold where
    show (Bold s) = "<b>" ++ s ++ "</b>"

data HtmlTagType = Open | Close | SelfClosing
    deriving (Eq)
data HtmlTag = HtmlTag {
    tagname :: String,
    attrs :: [Attr]
}
showInnerTag :: HtmlTag -> String
showInnerTag t = unwords (tagname t : (map show $ attrs t))

data Attr = Attr String String
instance Show Attr where
    show (Attr s t) = s ++ if (t /= "") then "=\"" ++ t ++ "\"" else ""

htmlTag :: HtmlTagType -> Parser HtmlTag
htmlTag tagType = do
    if tagType == Close then string "</" else string "<"
    spaces
    tagname <- many1 letter
    spaces
    attrs <- sepEndBy attr spaces
    if tagType == SelfClosing then string "/>" else string ">"
    return $ HtmlTag {tagname=tagname, attrs=attrs}

htmlContent :: Parser (Either String Html)
htmlContent = fmap Left (many1 $ noneOf "<") <|> fmap Right html

showHtmlContent :: Either String Html -> String
showHtmlContent (Left s) = s
showHtmlContent (Right h) = show h

data Html = PairTag HtmlTag [Either String Html] HtmlTag
          | SingleTag HtmlTag
instance Show Html where
    show (PairTag open content close) = "<" ++ showInnerTag open ++ ">" ++ concatMap showHtmlContent content ++ "</" ++ showInnerTag close ++ ">"
    show (SingleTag tag) = "<" ++ showInnerTag tag ++ "/>"

pairTag :: Parser Html
pairTag = do
    open <- htmlTag Open
    content <- option [] $ many $ try htmlContent
    close <- htmlTag Close
    return $ PairTag open content close

singleTag :: Parser Html
singleTag = fmap SingleTag $ htmlTag SelfClosing

html :: Parser Html
html = try pairTag <|> singleTag

attr :: Parser Attr
attr = do
    name <- many1 letter
    char '='
    val <- attrVal
    return $ Attr name val

attrVal :: Parser String
attrVal = between (char '"') (char '"') (many1 $ noneOf "\"")

italics' :: Parser String
italics' = between (char '*') (char '*') (many1 $ noneOf "*")

italics :: Parser Italics
italics = italics' >>= return . Italics

bold :: Parser Bold
bold = fmap Bold $ between (char '*') (char '*') italics'

inline :: Parser Inline
inline = choice [try (italics >>= return . InlineItalics),
                 bold >>= return . InlineBold]

main :: IO ()
main = do
    line <- getLine
    let result = parse html "test" line
    putStrLn $ show result
