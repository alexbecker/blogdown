module AST where

data AST = AST [Block] (Maybe FootnoteDefs)
    deriving (Show)

data FootnoteDefs = FootnoteDefs [FootnoteDef]
    deriving (Show)

data FootnoteDef = FootnoteDef {
    index :: Int,
    content :: [Block]
}
    deriving (Show)

data ListItem = ListItem [Inline]
              | SubList List
    deriving (Show)

data List = List Bool [ListItem]
    deriving (Show)

data TableCell = TableHeaderCell [Inline]
               | TableBodyCell [Inline]
    deriving (Show)

data TableRow = TableRow [TableCell]
    deriving (Show)

data Block = HardRule
           | Paragraph [Inline]
           | Header Int [Inline]
           | ListBlock List
           | BlockQuote [Inline]
           | BlockCode {
    codeClass :: String,
    codeContent :: String
}
           | BlockHtml Html
           | Table {
    thead :: Maybe [TableRow],
    body :: [TableRow]
}
    deriving (Show)

data Inline = Italics [Inline]
            | Bold [Inline]
            | Code String
            | FootnoteRef Int
            | Plaintext String
            | InlineHtml Html
            | Link {
    text :: [Inline],
    href :: String
}
            | Image {
    alt :: String,
    src :: String
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
