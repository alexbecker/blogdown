module AST where

import Text.Parsec hiding (Line)

data AST = AST [Block]

data Block = ParagraphBlock Paragraph

data Paragraph = Paragraph [Line]

data Line = Line [Inline]

data Inline = InlineItalics Italics
            | InlineBold Bold

data Italics = Italics String

data Bold = Bold String

data HtmlTagType = Open | Close | SelfClosing
    deriving (Eq)
data HtmlTag = HtmlTag {
    tagname :: String,
    attrs :: [Attr]
}

data Attr = Attr String String

data Html = PairTag HtmlTag [Either String Html] HtmlTag
          | SingleTag HtmlTag
