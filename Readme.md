*Note: If you are viewing this Readme on GitHub, its Blogdown-specific features will not render correctly.
The Blogdown-formatted output is in Readme.html.*

# Blogdown

Blogdown is a markup language based on Markdown, designed for writing blog posts.
Blogdown's goals are:
 * Clean syntax for common blog post features absent from Markdown.
 * Simple, well-defined parsing rules.
 * Ability to handle untrusted input.
 * Handling line breaks gracefully.
 * Compatibility with common Markdown implementations.

Because there is no Markdown standard and existing Markdown implementations
[disagree wildly even on simple cases](http://johnmacfarlane.net/babelmark2/?text=Hello+world%0A*+this+is+a+list%0A%3E+this+is+a+quote),
Blogdown cannot be 100% compatible with even a majority of Markdown implementations.
Given this, I decided it was worth it to make the syntax diverge slightly in exchange for consistency and simplicity.

## Installation

Blogdown requires [GHC](https://www.haskell.org/ghc/), [Parsec](https://hackage.haskell.org/package/parsec) and [MissingH](https://hackage.haskell.org/package/MissingH) to compile,
all of which are available through common Linux package managers.
Once these are installed, run `ghc -o Blogdown Blogdown.hs` in the repository's base directory.

## Usage

The `Blogdown` binary reads from `stdin` and writes to `stdout`. Typical usage looks like:

    cat blogpost.md | ./Blogdown > blogpost.html

### Optional Flags

`Blogdown` accepts the following long-style flags:
 * `--footnote-prefix`: Defines a prefix for the `id`s of footnotes. Recommended if multiple output files are included in a single HTML page, to avoid `id` collisions.
 * `--footnote-index-from`: The index from which footnotes are numbered. Defaults to 0.
 * `--footnote-backlinks`: If this flag is passed, footnotes will be preceded by a caret linking back to the point where the footnote is referenced.
 * `--em-dashes`: If this flag is passed, "--" will be replaced with "&mdash;" in text.

## Syntax

### Differences from Markdown

Most of the syntax of Blogdown should be familiar to Markdown users, but some new syntax has been added, and some existing syntax has changed.

#### New Features

Blogdown adds footnote support to Markdown.
Footnotes can be referenced inline with \^\[*footnote-name*\], which will render as a superscript link to a *footnote-definition*
at the end of the document, which is defined by \~\[*footnote-name*\] followed by the footnote contents.

#### Markdown Incompatibilities

Blogdown does not support the Markdown syntax of underlining text with '=' or '-' characters to define a header,
as this comes at a large cost in the parser implementation^[underline-parser-complexity].
The '#' syntax for headers is supported instead.

It also does not support using multiple trailing spaces to force a breakpoint at the end of a line.
The "&lt;br/&gt;" tag is supported instead.

The '\~' and '\^' characters are now special, and must be escaped to be used in text.

### Formal Description

The body of a Blogdown document consists of a sequence of *block nodes*, which in turn consist of *inline nodes*.

#### Block Nodes

Block nodes can contain any sequence of inline nodes, with the exception of code and HTML blocks, whose contents are rendered verbatim.
Block nodes can span multiple lines and are terminated by a blank line, the beginning of another type of block, or the end of the document.

The following block node types are supported:
 * **Paragraph**: The default block type; any content not in another block is part of a paragraph.
Paragraphs must be separated by a blank line.
Can contain arbitrary inline nodes.
 * **Header**: 1-6 '#' characters at the beginning of a line begins a header, with the number of '#' characters determining the header level.
Can contain arbitrary inline nodes.
 * **Ordered Lists**: A " \* " begins an ordered list item, which itself is a block.
Sequential ordered list items form an ordered list.
Can contain arbitrary inline nodes.
 * **Unordered Lists**: A " \* " begins an unordered list item, which itself is a block.
Sequential unordered list items form an unordered list.
Can contain arbitrary inline nodes.
 * **Blockquote**: Lines beginning with "&gt; " define a blockquote.
Can contain arbitrary inline nodes.
Note that the first line not beginning with "&gt; " will start a new block.
 * **Code Block**: Lines indented with 4+ spaces or a tab define a code block.
Code blocks are rendered verbatim, ignoring special characters.
Note that the first un-indented line will start a new block.
 * **HTML Block**: An HTML tag at the beginning of a line starts an HTML block.
Its contents must be valid HTML, and it is ended by the corresponding closing tag.
HTML blocks are rendered verbatim, unless HTML bleaching is enabled.
 * **Hard Rule**: A line consisting of 3+ "-" defines a hard rule.

#### Inline Nodes

Inline nodes can generally contain a sequence of other inline nodes, but cannot contain nodes of the same type.
Despite the name, inline nodes can span multiple lines, e.g. to accomodate line length limits.

The following inline node types are supported:
 * **Plaintext**: The default inline type; any text not in another inline node is plaintext. Rendered verbatim.
 * **Italic**: Surrounding text with "\*" *italicizes* it. Italic nodes can contain any other type of inline node.
 * **Bold**: Surrounding text with "\*\*" **bolds** it. Bold nodes can contain any other type of inline node.
 * **Code**: Surrounding text with "\`" renders it as `code`. The content is rendered verbatim.
 * **Link**: A [link](#) is written as "\[*text*\]\(*href*\)". The *text* portion can contain any other type of inline node.
The *href* portion is the link destination, and is parsed verbatim except that any literal "\(" or "\)" must be escaped.
 * **Footnote Reference**: Writing \^\[*footnote-name*\] defines a footnote reference.
It is rendered as a superscript footnote number^[footnote-numbering], and links to the footnote named *footnote-name*
if it is present in the footer.

#### Footer

A Blogdown document may optionally include a footer after the body.
The footer consists of a sequence of *footnote definitions*, each of which begins on a new line with \~\[*footnote-name*\] and consists of an arbitrary sequence of blocks.
A footnote definition is only terminated by another footnote definition or the end of the document.

## Planned improvements
 * Tables
 * Better error messages on parse failures
 * Windows support

---

~[underline-parser-complexity] Supporting underlines for headers requires the parser to look-ahead arbitrarily far, resulting in quadratic time complexity.

~[footnote-numbering] Footnotes are auto-numbered in order of appearance, starting from 0 by default (this can be changed by passing the `--footnote-index-from` flag).
