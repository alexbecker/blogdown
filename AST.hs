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
            | InlineFootnoteRef FootnoteRef
    deriving (Show)

data LinkContents = LCItalics Italics
                  | LCBold Bold
                  | LCCode Code
                  | LCHtml Html
                  | Plaintext String
    deriving (Show)

data Italics = Italics String
    deriving (Show)

data Bold = Bold String
    deriving (Show)

data Code = Code String
    deriving (Show)

data Link = Link {
    text :: LinkContents,
    href :: String
}
    deriving (Show)

data FootnoteRef = FootnoteRef String
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
