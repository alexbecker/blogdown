module AST where

import Text.Parsec hiding (Line)

data AST = AST [Block]
    deriving (Show)

data Block = Paragraph [Line]
           | Header Int Line
           | UnorderedList [Line]
           | BlockQuote [Line]
           | BlockCode [String]
           | FootnoteDefs [FootnoteDef]
           | BlockHtml Html
    deriving (Show)

data FootnoteDef = FootnoteDef String [Line]
    deriving (Show)

data Line = Line [Inline]
    deriving (Show)

data Inline = Italics Inline
            | Bold Inline
            | Code String
            | FootnoteRef String
            | Plaintext String
            | InlineHtml Html
            | InlineLink Link
    deriving (Show)

data Link = Link {
    text :: [Inline],
    href :: String
}
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

data Html = PairTag HtmlTag [Either String Html]
          | SingleTag HtmlTag
    deriving (Show)
