module AST where

import Text.Parsec hiding (Line)

data AST = AST [Block] (Maybe FootnoteDefs)
    deriving (Show)

data FootnoteDefs = FootnoteDefs [FootnoteDef]
    deriving (Show)

data FootnoteDef = FootnoteDef {
    identifier :: String,
    content :: [Block]
}
    deriving (Show)

data UnorderedListItem = UnorderedListItem [Inline]
    deriving (Show)

data Block = HardRule
           | Paragraph [Inline]
           | Header Int [Inline]
           | UnorderedList [UnorderedListItem]
           | BlockQuote [Inline]
           | BlockCode [String]
           | BlockHtml Html
    deriving (Show)

data Inline = Italics [Inline]
            | Bold [Inline]
            | Code String
            | FootnoteRef String
            | Plaintext String
            | InlineHtml Html
            | Link {
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
