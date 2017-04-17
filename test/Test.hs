module Main where

import Data.String.Utils
import Text.Parsec
import System.Exit

import Parsing.Parse
import Parsing.ParseBlock
import Parsing.ParseHtml
import Parsing.ParseInline
import Parsing.State
import Rendering.Render
import Rendering.RenderOptions

printExpectedSuccess :: (ToHtml a) => String -> String -> String -> a -> IO Bool
printExpectedSuccess name input expected parsed = if output == expected
    then do
        putStrLn $ "PASS: " ++ name
        return True
    else do
        putStrLn $ "FAIL: " ++ name
        putStrLn "in:"
        putStrLn input
        putStrLn "out:"
        putStrLn output
        putStrLn "expect:"
        putStrLn expected
        return False
    where
        output = toHtml defaultRenderOptions parsed

expectSuccess :: (ToHtml a) => String -> (Parser a) -> String -> String -> IO Bool
expectSuccess name p input expected = either
    (\err -> do
        putStrLn $ "FAIL: " ++ name
        putStrLn $ show err
        return False)
    (printExpectedSuccess name input expected)
    $ runParser p initialState name input

testItalics = expectSuccess "italics" inline "*abc*" "<i>abc</i>"
testBold = expectSuccess "bold" inline "**abc**" "<b>abc</b>"
testBoldItalics = expectSuccess "bold italics" inline "***abc***" "<b><i>abc</i></b>"
testCode = expectSuccess "code" inline "`abc`" "<code>abc</code>"
testInlineHtml = expectSuccess "inline html" html
    "<abbr title=\"\">SQL</abbr>"
    "<abbr title=\"\">SQL</abbr>"
testMultipleAttrs = expectSuccess "html with multiple attrs" html
    "<abbr title=\"a\" id=\"x\">SQL</abbr>"
    "<abbr title=\"a\" id=\"x\">SQL</abbr>"
testFootnoteRef = expectSuccess "footnote reference" inline
    "^[x]"
    "<sup><a href=\"#-footnote-0\" id=\"a--footnote-0\">[0]</a></sup>"
testCaret = expectSuccess "literal '^' does not need escaping" inline "^" "^"
testImage = expectSuccess "inline image" inline
    "![an image](/img/0)"
    "<img alt=\"an image\" src=\"/img/0\"/>"
testExclamationMark = expectSuccess "literal '!' does not need escaping" inline "!" "!"
testLink = expectSuccess "link" inline
    "[Google](https://google.com)"
    "<a href=\"https://google.com\">Google</a>"
testLinkWithContents = expectSuccess "link with styling inside" inline
    "[*Whence* `he` **came**](https://google.com)"
    "<a href=\"https://google.com\"><i>Whence</i> <code>he</code> <b>came</b></a>"
testLinkImplicit = expectSuccess "link with implicit href" inline
    "[https://google.com]"
    "<a href=\"https://google.com\">https://google.com</a>"
testH1 = expectSuccess "h1" header "# hello" "<h1>hello</h1>"
testH6 = expectSuccess "h6" header "###### hello" "<h6>hello</h6>"
testHardRule = expectSuccess "hard rule" hardRule "---\n" "<hr/>"
testHardRuleLong = expectSuccess "hard rule with extra dashes" hardRule "----\n" "<hr/>"
testParagraph = expectSuccess "paragraph" paragraph
    "This is a paragraph\n\
    \of text.\n"
    "<p>This is a paragraph\n\
    \of text.</p>"
testEscapeCharacters = expectSuccess "paragraph with escaped special characters" paragraph
    "These are special: \\~, \\*, \\[, \\], \\^\n"
    "<p>These are special: ~, *, [, ], ^</p>"
testHashInParagraph = expectSuccess "paragraph containing literal '#'" paragraph
    "This is a paragraph\n\
    \containing '#'\n"
    "<p>This is a paragraph\n\
    \containing '#'</p>"
testOrderedList = expectSuccess "ol" listBlock
    " - point 1\n\
    \ - point 2\n"
    "<ol><li>point 1</li>\n\
    \<li>point 2</li>\n\
    \</ol>"
testUnorderedList = expectSuccess "ul" listBlock
    " * point 1\n\
    \ * point 2\n"
    "<ul><li>point 1</li>\n\
    \<li>point 2</li>\n\
    \</ul>"
testNestedList = expectSuccess "nested list" listBlock
    " - point 1\n\
    \ - point 2\n\
    \  * point 3\n\
    \  * point 4\n\
    \   - point 5\n\
    \  * point 6\n\
    \ - point 7\n"
    "<ol><li>point 1</li>\n\
    \<li>point 2</li>\n\
    \<ul><li>point 3</li>\n\
    \<li>point 4</li>\n\
    \<ol><li>point 5</li>\n\
    \</ol>\n\
    \<li>point 6</li>\n\
    \</ul>\n\
    \<li>point 7</li>\n\
    \</ol>"
testBlockQuote = expectSuccess "blockquote" blockQuote
    "> The politician said that\n\
    \> he would fix the economy.\n"
    "<blockquote>The politician said that\n\
    \he would fix the economy.</blockquote>"
testBlockQuotePreFormatted = expectSuccess "blockquote pre-formatted" blockQuote
    ">     \"This is it... this is where I belong...\"\n\
    \>     I know everyone here... even if I've never met them, never talked to\n\
    \> them, may never hear from them again... I know you all...\n"
    "<blockquote>    \"This is it... this is where I belong...\"\n\
    \    I know everyone here... even if I've never met them, never talked to\n\
    \them, may never hear from them again... I know you all...</blockquote>"
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
testBlockCodeSpecialChars = expectSuccess "any line beginning with four spaces should be a block of code, regardless of the first non-whitespace character" block
    "    > print(1)\n\
    \    * 1"
    "<pre><code>&gt; print(1)\n\
    \* 1\n\
    \</code></pre>"
testBlockHtml = expectSuccess "block html" blockHtml
    "<div class=\"class\">\n\
    \    <span></span>\n\
    \</div>"
    "<div class=\"class\">\n\
    \    <span></span>\n\
    \</div>"
testTable = expectSuccess "table" table
    "+---+---+\n\
    \| a | b |\n\
    \| c | d |\n\
    \+---+---+\n"
    "<table><tbody><tr><td> a </td>\n\
    \<td> b </td>\n\
    \</tr>\n\
    \<tr><td> c </td>\n\
    \<td> d </td>\n\
    \</tr>\n\
    \</tbody></table>"
testTableHeader = expectSuccess "table with header" table
    "+---+---+\n\
    \| a | b |\n\
    \+---+---+\n\
    \| c | d |\n\
    \| e | f |\n\
    \+---+---+\n"
    "<table><thead><tr><th> a </th>\n\
    \<th> b </th>\n\
    \</tr>\n\
    \</thead><tbody><tr><td> c </td>\n\
    \<td> d </td>\n\
    \</tr>\n\
    \<tr><td> e </td>\n\
    \<td> f </td>\n\
    \</tr>\n\
    \</tbody></table>"
testTableMinimal = expectSuccess "minimal table" table
    "| a | b |\n\
    \| c | d |\n"
    "<table><tbody><tr><td> a </td>\n\
    \<td> b </td>\n\
    \</tr>\n\
    \<tr><td> c </td>\n\
    \<td> d </td>\n\
    \</tr>\n\
    \</tbody></table>"
testTableHeaderMinimal = expectSuccess "minimal table with header" table
    "| a | b |\n\
    \+\n\
    \| c | d |\n\
    \| e | f |\n"
    "<table><thead><tr><th> a </th>\n\
    \<th> b </th>\n\
    \</tr>\n\
    \</thead><tbody><tr><td> c </td>\n\
    \<td> d </td>\n\
    \</tr>\n\
    \<tr><td> e </td>\n\
    \<td> f </td>\n\
    \</tr>\n\
    \</tbody></table>"
testFootnoteDef = expectSuccess "footnote definition" ast
    "^[x]\n\
    \~[x] This is a single list item\n\
    \of footnote.\n"
    "<p><sup><a href=\"#-footnote-0\" id=\"a--footnote-0\">[0]</a></sup></p>\n\
    \<ol start=\"0\" class=\"footnotes\"><li id=\"-footnote-0\"><p>This is a single list item\n\
    \of footnote.</p></li>\n\
    \</ol>\n"
testFootnoteDefTwoLines = expectSuccess "footnote definition, separated by 2 lines" ast
    "^[x]\n\
    \\n\
    \~[x] This is a single list item\n\
    \of footnote.\n"
    "<p><sup><a href=\"#-footnote-0\" id=\"a--footnote-0\">[0]</a></sup></p>\n\
    \<ol start=\"0\" class=\"footnotes\"><li id=\"-footnote-0\"><p>This is a single list item\n\
    \of footnote.</p></li>\n\
    \</ol>\n"
testFootnoteDefs = expectSuccess "multiple footnote definitions" ast
    "^[0]^[1]^[2]\n\
    \~[0] a\n\
    \~[1] b\n\
    \\n\
    \~[2] c\n"
    "<p><sup><a href=\"#-footnote-0\" id=\"a--footnote-0\">[0]</a></sup><sup><a href=\"#-footnote-1\" id=\"a--footnote-1\">[1]</a></sup><sup><a href=\"#-footnote-2\" id=\"a--footnote-2\">[2]</a></sup></p>\n\
    \<ol start=\"0\" class=\"footnotes\"><li id=\"-footnote-0\"><p>a</p></li>\n\
    \<li id=\"-footnote-1\"><p>b</p></li>\n\
    \<li id=\"-footnote-2\"><p>c</p></li>\n\
    \</ol>\n"
testFootnoteOrdering = expectSuccess "footnotes should be sorted by first reference" ast
    "^[0]^[1]\n\
    \~[1] b\n\
    \~[0] a\n"
    "<p><sup><a href=\"#-footnote-0\" id=\"a--footnote-0\">[0]</a></sup><sup><a href=\"#-footnote-1\" id=\"a--footnote-1\">[1]</a></sup></p>\n\
    \<ol start=\"0\" class=\"footnotes\"><li id=\"-footnote-0\"><p>a</p></li>\n\
    \<li id=\"-footnote-1\"><p>b</p></li>\n\
    \</ol>\n"
testContinuation = expectSuccess "continuation character" paragraph
    "a\\\nb"
    "<p>ab</p>"

expectFailure :: (ToHtml a) => String -> (Parser a) -> String -> String -> IO Bool
expectFailure name p input expectedErr = either
    (\err -> if endswith expectedErr $ show err
        then do
            putStrLn $ "PASS: " ++ name
            return True
        else do
            putStrLn $ "FAIL: " ++ name
            putStrLn "in:"
            putStrLn input
            putStrLn "out:"
            putStrLn $ show err
            putStrLn "expect:"
            putStrLn expectedErr
            return False)
    (\parsed -> do
        putStrLn $ "FAIL: " ++ name
        putStrLn $ "unexpected success:"
        putStrLn $ toHtml defaultRenderOptions parsed
        return False)
    $ runParser p initialState name input

testNestedBold = expectFailure "bold tags cannot be nested" inline
    "****abc****"
    "cannot have empty or nested bold nodes"
testMismatchedBoldItalics = expectFailure "bold opening tag closed with italics tag" inline
    "**a*"
    "expecting content in italics node or extra \"*\" to close bold node"
testSwappedItalicsBold = expectFailure "italics and bold closing tags swapped" paragraph
    "*a**b*c**"
    "unexpected \"c\"\n\
    \expecting closing \"**\" (bold)"
testNestedLink = expectFailure "links cannot be nested" inline
    "[[a](https://a.com)](https://b.com)"
    "unexpected \"[\"\n\
    \expecting \"**\" (bold), \"*\" (italics), \"`\" (code), \"^[\" (footnote reference), \"![\" (image) or \"<\" (html tag)\n\
    \links cannot be nested"
testBadImplicitLink = expectFailure "link href required unless text is valid URI" inline
    "[notauri]"
    "unexpected end of input\n\
    \expecting \"(\" (link href)\n\
    \link href is required unless link text is a valid absolute URI"
testUnclosedOpeningTag = expectFailure "unclosed opening tag should fail to parse" html
    "<div"
    "unexpected end of input\n\
    \expecting rest of tag name, space or closing \">\" (html tag)"
testUnclosedTag = expectFailure "unclosed tag should fail to parse" html
    "<p>hello"
    "unexpected end of input\n\
    \expecting \"<\" (html tag) or \"</\" (closing html tag)"
testMismatchedTags = expectFailure "mismatched tags should fail to parse" html
    "<a>hello</b>"
    "mismatched tags: 'a' and 'b'"
testBadTableSeparator = expectFailure "incomplete table separator" block
    "+---+---\n\
    \| a | b |"
    "unexpected \"\\n\"\n\
    \expecting \"-\" or \"+\" (table)"

goldenTest :: FilePath -> FilePath -> String -> IO Bool
goldenTest inFilePath goldenFilePath renderArgs = do
    let r = renderOptions $ words renderArgs
    input <- readFile inFilePath
    golden <- readFile goldenFilePath
    either
        (\err -> do
            putStrLn $ "FAIL: " ++ goldenFilePath
            putStrLn $ show err
            return False)
        (\parsed -> do
            let rendered = toHtml r parsed
            if rendered == golden
                then do
                    putStrLn $ "PASS: " ++ goldenFilePath
                    return True
                else do
                    let failPath = goldenFilePath ++ ".fail"
                    writeFile failPath rendered
                    putStrLn $ "FAIL: " ++ goldenFilePath
                    return False)
        $ runParser ast initialState inFilePath input

main :: IO ()
main = do
    results <- sequence [
        testItalics,
        testBold,
        testBoldItalics,
        testCode,
        testInlineHtml,
        testMultipleAttrs,
        testFootnoteRef,
        testLink,
        testLinkWithContents,
        testLinkImplicit,
        testCaret,
        testImage,
        testExclamationMark,
        testH1,
        testH6,
        testHardRule,
        testHardRuleLong,
        testParagraph,
        testEscapeCharacters,
        testHashInParagraph,
        testOrderedList,
        testUnorderedList,
        testNestedList,
        testBlockQuote,
        testBlockQuotePreFormatted,
        testBlockCode,
        testBlockCodeWhitespace,
        testBlockCodeSpecialChars,
        testBlockHtml,
        testTable,
        testTableHeader,
        testTableMinimal,
        testTableHeaderMinimal,
        testFootnoteDef,
        testFootnoteDefTwoLines,
        testFootnoteDefs,
        testFootnoteOrdering,
        testContinuation,
        testNestedBold,
        testMismatchedBoldItalics,
        testSwappedItalicsBold,
        testNestedLink,
        testBadImplicitLink,
        testUnclosedOpeningTag,
        testUnclosedTag,
        testMismatchedTags,
        testBadTableSeparator,
        goldenTest "Readme.md" "test/goldens/Readme.html" "--em-dashes --inline-css --inline-js",
        goldenTest "test/goldens/golden1.md" "test/goldens/golden1.html" "--em-dashes",
        goldenTest "test/goldens/golden1.md" "test/goldens/golden1-backlinks.html" "--em-dashes --footnote-backlinks",
        goldenTest "test/goldens/golden2.md" "test/goldens/golden2.html" "--em-dashes",
        goldenTest "test/goldens/golden2.md" "test/goldens/golden2-backlinks.html" "--em-dashes --footnote-backlinks"]
    if and results
        then exitSuccess
        else exitFailure
