module AST where

import Text.Parsec hiding (Line)

data AST = AST [Block]
    deriving (Show)

data Block = ParagraphBlock Paragraph
    deriving (Show)

data Paragraph = Paragraph [Line]
    deriving (Show)

data Line = Line [Inline]
    deriving (Show)

data Inline = InlineItalics Italics
            | InlineBold Bold
            | InlineHtml Html
            | Plaintext String
    deriving (Show)

data Italics = Italics String
    deriving (Show)

data Bold = Bold String
    deriving (Show)

data HtmlTagType = Open | Close | SelfClosing
    deriving (Show, Eq)
data HtmlTag = HtmlTag {
    tagname :: String,
    attrs :: [Attr]
}
    deriving (Show)

data Attr = Attr String String
    deriving (Show)

data Html = PairTag HtmlTag [Either String Html] HtmlTag
          | SingleTag HtmlTag
    deriving (Show)
