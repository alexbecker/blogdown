module Main where

import Data.Either
import Text.Parsec
import Text.Parsec.Error

import Parse
import Render
import RenderOptions

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
        output = render defaultRenderOptions parsed

expectSuccess :: (ToHtml a) => String -> (Parser a) -> String -> String -> IO ()
expectSuccess name p input expected = either
    ((flip const (putStrLn $ "FAIL: " ++ name) >> putStrLn) . show)
    (printExpectedSuccess name input expected)
    $ parse p name input

testItalics = expectSuccess "italics" italics "*abc*" "<i>abc</i>"
testBold = expectSuccess "bold" bold "**abc**" "<b>abc</b>"
testBoldItalics = expectSuccess "bold italics" bold "***abc***" "<b><i>abc</i></b>"
testCode = expectSuccess "code" code "`abc`" "<code>abc</code>"
testInlineHtml = expectSuccess "inline html" html
    "<abbr title=\"\">SQL</abbr>"
    "<abbr title=\"\">SQL</abbr>"
testFootnoteRef = expectSuccess "footnote reference" footnoteRef
    "^[x]"
    "<sup><a href=\"#-footnote-x\" id=\"a--footnote-x\">[0]</a></sup>"
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
testHardRule = expectSuccess "hard rule" hardRule "---\n" "<hr/>"
testHardRuleLong = expectSuccess "hard rule with extra dashes" hardRule "----\n" "<hr/>"
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
    "> The politician said that\n\
    \> he would fix the economy.\n"
    "<blockquote>The politician said that\n\
    \he would fix the economy.</blockquote>"
testBlockCode = expectSuccess "block code" blockCode
    "    var x = 0;\n\
    \    alert(x);\n"
    "<pre><code>var x = 0;\n\
    \alert(x);\n\
    \</code></pre>"
testBlockCodeWhitespace = expectSuccess "block code handles starting whitespace correctly" blockCode
    "    def f(x):\n\
    \        return x\n"
    "<pre><code>def f(x):\n\
    \    return x\n\
    \</code></pre>"
testBlockCodeSpecialChars = expectSuccess "any line beginning with four spaces should be ablock of code, regardless of the first non-whitespace character" block
    "    > print(1)\n\
    \    * 1"
    "<pre><code>&gt; print(1)\n\
    \* 1\n\
    \</code></pre>"
testFootnoteDef = expectSuccess "footnote definition" footnoteDef
    "~[x] This is a single list item\n\
    \of footnote.\n"
    "<li id=\"-footnote-x\"><p>This is a single list item\n\
    \of footnote.</p></li>"
testBlockHtml = expectSuccess "block html" blockHtml
    "<div class=\"class\">\n\
    \    <span></span>\n\
    \</div>"
    "<div class=\"class\">\n\
    \    <span></span>\n\
    \</div>"
testFootnoteOrdering = expectSuccess "footnotes should be sorted by first reference" ast
    "^[0]^[1]\n\
    \~[1] b\n\
    \~[0] a\n"
    "<p><sup><a href=\"#-footnote-0\" id=\"a--footnote-0\">[0]</a></sup><sup><a href=\"#-footnote-1\" id=\"a--footnote-1\">[1]</a></sup></p>\n\
    \<ol start=\"0\" class=\"footnotes\"><li id=\"-footnote-0\"><p>a</p></li>\n\
    \<li id=\"-footnote-1\"><p>b</p></li>\n\
    \</ol>\n"
testAST = expectSuccess "whole AST" ast
    "# hello\n\
    \This is a paragraph\n\
    \of text.\n\
    \\n\
    \Now *with* some^[x] `styling`\n\
    \**behind** [it](https://google.com)\n\
    \ * point 1\n\
    \ * point 2\n\
    \> The politician said that\n\
    \> he would fix the economy.\n\
    \    var x = 0;\n\
    \    alert(x);\n\
    \\n\
    \This is a paragraph.\n\
    \There are many like it but this one is mine.\n\
    \<div class=\"class\">\n\
    \    <span>foo</span>\n\
    \</div>\n\
    \\n\
    \---\n\
    \~[x] This is a paragraph\n\
    \of footnote.\n"
    "<h1>hello</h1>\n\
    \<p>This is a paragraph\n\
    \of text.</p>\n\
    \<p>Now <i>with</i> some<sup><a href=\"#-footnote-x\" id=\"a--footnote-x\">[0]</a></sup> <code>styling</code>\n\
    \<b>behind</b> <a href=\"https://google.com\">it</a></p>\n\
    \<ul><li>point 1</li>\n\
    \<li>point 2</li>\n\
    \</ul>\n\
    \<blockquote>The politician said that\n\
    \he would fix the economy.</blockquote>\n\
    \<pre><code>var x = 0;\n\
    \alert(x);\n\
    \</code></pre>\n\
    \<p>This is a paragraph.\n\
    \There are many like it but this one is mine.\n\
    \<div class=\"class\">\n\
    \    <span>foo</span>\n\
    \</div></p>\n\
    \<hr/>\n\
    \<ol start=\"0\" class=\"footnotes\"><li id=\"-footnote-x\"><p>This is a paragraph\n\
    \of footnote.</p></li>\n\
    \</ol>\n"

expectFailure :: String -> (Parser a) -> String -> IO ()
expectFailure name p input = either
    (const $ putStrLn $ "PASS: " ++ name)
    (const $ putStrLn $ "FAIL: " ++ name)
    $ parse p name input

testNestedBold = expectFailure "bold tags cannot be nested" bold "****abc****"
testNestedLink = expectFailure "links cannot be nested" link "[[a](https://a.com)](https://b.com)"
testUnclosedTag = expectFailure "unclosed tag should fail to parse" html "<p>hello"
testMismatchedTags = expectFailure "mismatched tags should fail to parse" html "<a>hello</b>"

main :: IO ()
main = do
    testItalics
    testBold
    testBoldItalics
    testCode
    testInlineHtml
    testFootnoteRef
    testLink
    testLinkWithContents
    testLine
    testH1
    testH6
    testHardRule
    testHardRuleLong
    testParagraph
    testUnorderedList
    testBlockQuote
    testBlockCode
    testBlockCodeWhitespace
    testBlockCodeSpecialChars
    testFootnoteDef
    testBlockHtml
    testFootnoteOrdering
    testAST
    testNestedBold
    testNestedLink
    testUnclosedTag
    testMismatchedTags
