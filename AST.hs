module AST where

import Text.Parsec hiding (Line)

data AST = AST [Block]
    deriving (Show)

data Block = Paragraph [Line]
           | Header Int Line
           | UnorderedList [Line]
           | BlockQuote [Line]
           | BlockCode [String]
           | FootnoteDef String [Line]
    deriving (Show)

data Line = Line [Inline]
    deriving (Show)

data Inline = InlineLink Link
            | InlineNonLink LinkContents
    deriving (Show)

data LinkContents = Italics String
                  | Bold String
                  | Code String
                  | FootnoteRef String
                  | Plaintext String
                  | Html' Html
    deriving (Show)

data Link = Link {
    text :: LinkContents,
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

data Html = PairTag HtmlTag [Either String Html] HtmlTag
          | SingleTag HtmlTag
    deriving (Show)
