module Main where

import Data.Either
import Text.Parsec
import Text.Parsec.Error

import Parse
import Render

printExpectedSuccess :: (ToHtml a) => String -> String -> String -> a -> IO ()
printExpectedSuccess name input expected parsed = if output == expected
    then putStrLn $ "PASS: " ++ name
    else do
        putStrLn $ "FAIL: " ++ name
        putStrLn "in:"
        putStrLn input
        putStrLn "out:"
        putStrLn output
        putStrLn "expect:"
        putStrLn expected
    where
        output = render parsed

expectSuccess :: (ToHtml a) => String -> (Parser a) -> String -> String -> IO ()
expectSuccess name p input expected = either
    ((const (putStrLn $ "FAIL: " ++ name) >> putStrLn) . show)
    (printExpectedSuccess name input expected)
    $ parse p name input

testItalics = expectSuccess "italics" italics "*abc*" "<i>abc</i>"
testBold = expectSuccess "bold" bold "**abc**" "<b>abc</b>"
testCode = expectSuccess "code" code "`abc`" "<code>abc</code>"
testInlineHtml = expectSuccess "inline html" html "<abbr>SQL</abbr>" "<abbr>SQL</abbr>"
testLink = expectSuccess "link" link
    "[Google](https://google.com)"
    "<a href=\"https://google.com\">Google</a>"
testLinkWithContents = expectSuccess "link with styling inside" link
    "[*Whence* `he` **came**](https://google.com)"
    "<a href=\"https://google.com\"><i>Whence</i> <code>he</code> <b>came</b></a>"
testLine = expectSuccess "line" line
    "hi *there* **bob**"
    "hi <i>there</i> <b>bob</b>"
testH1 = expectSuccess "h1" header "# hello" "<h1>hello</h1>"
testH6 = expectSuccess "h6" header "###### hello" "<h6>hello</h6>"
testParagraph = expectSuccess "paragraph" paragraph
    "This is a paragraph\n\
    \of text.\n"
    "<p>This is a paragraph\n\
    \of text.</p>"
testUnorderedList = expectSuccess "ul" unorderedList
    " * point 1\n\
    \ * point 2\n"
    "<ul><li>point 1</li>\n\
    \<li>point 2</li>\n</ul>"
testBlockQuote = expectSuccess "blockquote" blockQuote
    " > The politician said that\n\
    \ > he would fix the economy.\n"
    "<blockquote>The politician said that\n\
    \he would fix the economy.</blockquote>"
testBlockCode = expectSuccess "block code" blockCode
    "    var x = 0;\n\
    \    alert(x);\n"
    "<pre><code>var x = 0;\n\
    \alert(x);\n</code></pre>"

expectFailure :: String -> (Parser a) -> String -> [String] -> IO ()
expectFailure name p input expected = either
    (const $ putStrLn $ "PASS: " ++ name)
    (const $ putStrLn $ "FAIL: " ++ name)
    $ parse p name input

testUnclosedTag = expectFailure "unclosed tag should fail to parse" html "<p>hello" []

main :: IO ()
main = do
    testItalics
    testBold
    testCode
    testInlineHtml
    testLink
    testLinkWithContents
    testLine
    testH1
    testH6
    testParagraph
    testUnorderedList
    testBlockQuote
    testBlockCode
    testUnclosedTag
