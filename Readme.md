*Note: If you are viewing this Readme on GitHub, its Blogdown-specific features will not render correctly.
The Blogdown-formatted output is in `test/goldens/Readme.html`.*

# [Blogdown](https://blogdown.io)

Blogdown is a markup language based on Markdown, designed for writing blog posts.
Blogdown's goals are:
 * Clean syntax for common blog post features absent from Markdown.
 * Ability to handle untrusted input.
 * Simple syntax, with no surprises.
 * Debuggability.
 * Near compatibility with common Markdown implementations.

Because there is no Markdown standard and existing Markdown implementations
[disagree wildly even on simple cases](http://johnmacfarlane.net/babelmark2/?text=Hello+world%0A*+this+is+a+list%0A%3E+this+is+a+quote),
Blogdown cannot be 100% compatible with even a majority of Markdown implementations.
While there have been attemps to create a common Markdown standard--most notably [CommonMark](http://commonmark.org/)--they
are necessarily quite complex. The primary cause of this complexity is that Markdown insists on rendering *something* for every input,
no matter how malformed. Blogdown is considerably simpler, and hopefully easier for authors to debug, because it fails on malformed inputs.
With full compatability out of the window, I have chosen to make some other small improvements on Markdown syntax.

## Installation

### With Cabal or Stack

The recommended way to install any Haskell project is using [Cabal](https://www.haskell.org/cabal/) or [Stack](https://docs.haskellstack.org/en/stable/README/).
With these tools, you can simply run `cabal install` or `stack build` respectively.

### Without Haskell Tooling

Because configuring Cabal and Stack can be tricky for Haskell beginners, Blogdown supports installation without any Haskell tooling.

First, install [GHC](https://www.haskell.org/ghc/), [Parsec](https://hackage.haskell.org/package/parsec) and [MissingH](https://hackage.haskell.org/package/MissingH),
all of which are available through common Linux package managers.
Then run `ghc -isrc -o Blogdown src/Blogdown.hs` in the repository's base directory.

## Usage

The `Blogdown` binary reads from `stdin` and writes to `stdout`. Typical usage looks like:

    cat blogpost.md | ./Blogdown > blogpost.html

### Optional Styling and Scripts

It is recommended to include `footnotes.css` and `footnotes.js` on any pages which make use of Blogdown-generated footnotes,
which improve the appearance of footnotes and allow them to be shown inline.
These can be inlined using the `--inline-css` and `--inline-js` flags respectively^[inline].

### Optional Flags

`Blogdown` accepts the following long-style flags:
 * `--footnote-prefix`: Defines a prefix for the `id`s of footnotes. Recommended if multiple output files are included in a single HTML page, to avoid `id` collisions.
 * `--footnote-index-from`: The index from which footnotes are numbered. Defaults to 0.
 * `--footnote-backlinks`: If this flag is passed, footnotes will be preceded by a caret linking back to the point where the footnote is referenced.
 * `--em-dashes`: If this flag is passed, `--` will be replaced with "&mdash;" in text.
 * `--inline-css`: If this flag is passed, the recommended CSS will be inlined at the end of the output document.
 * `--inline-js`: If this flag is passed, the recommended JS will be inlined at the end of the output document.

**Note:** The `inline-*` options require additional files (located in `assets`) to function.
These will be installed at a known location if built with Cabal or Stack,
but if the binary is run on a different machine (e.g. a webserver) then the binary is built on,
the machine must also have the `assets` directory, and the directory containing `assets` should be supplied via the environment variable `blogdown_datadir_override`.

## Syntax

### Differences from Markdown

Most of the syntax of Blogdown should be familiar to Markdown users, but some new syntax has been added, and some existing syntax has changed.

#### New Features

Blogdown adds footnote support to Markdown.
Footnotes can be referenced inline with \^\[*footnote-name*\], which will render as a superscript link to a *footnote-definition*
at the end of the document, which is defined by \~\[*footnote-name*\] followed by the footnote contents.

#### Markdown Incompatibilities

Blogdown does not support the Markdown syntax of underlining text with `=` or `-` characters to define a header,
as this comes at a large cost in the parser implementation^[underline-parser-complexity].
The `#` syntax for headers is supported instead.

It also does not support using multiple trailing spaces to force a breakpoint at the end of a line.
The `<br/>` tag is supported instead.

Since Blogdown introduces new syntax, some valid Markdown will require escaping to render as expected in Blogdown.
Additionally, while most Markdown implementations do not require escaping many special characters when their special meaning would
not be valid, Blogdown always requires they be escaped.

### Formal Description

The body of a Blogdown document consists of a sequence of *block nodes*, which in turn consist of *inline nodes*.

#### Block Nodes

Block nodes can contain any sequence of inline nodes, with the exception of code and HTML blocks, whose contents are rendered verbatim.
Block nodes can span multiple lines and are terminated by a blank line, the beginning of another type of block, or the end of the document.

The following block node types are supported:
 * **Paragraph**: The default block type; any content not in another block is part of a paragraph.
Paragraphs must be separated by a blank line.
Can contain arbitrary inline nodes.
 * **Header**: 1-6 `#` characters at the beginning of a line begins a header, with the number of `#` characters determining the header level.
Can contain arbitrary inline nodes.
 * **Ordered Lists**: A ` - ` begins an ordered list item, which itself is a block.
Sequential ordered list items or sublists form an ordered list.
List items can contain arbitrary inline nodes.
  * Sublists are created when list items begin with one more space than their parent list, and can be ordered or unordered.
For example, this sublist begins with `  * `.
 * **Unordered Lists**: A ` * ` begins an unordered list item, which itself is a block.
Sequential unordered list items or sublists form an unordered list.
List items can contain arbitrary inline nodes.
  * Sublists are created when list items begin with one more space than their parent list, and can be ordered or unordered.
For example, this sublist begins with `  * `.
 * **Blockquote**: Lines beginning with `> ` define a blockquote.
Can contain arbitrary inline nodes.
Note that the first line not beginning with `> ` will start a new block.
 * **Code Block**: Lines indented with 4+ spaces or a tab define a code block.
Code blocks are rendered verbatim, ignoring special characters.
Note that the first un-indented line will start a new block.
 * **HTML Block**: An HTML tag at the beginning of a line starts an HTML block.
Its contents must be valid HTML, and it is ended by the corresponding closing tag.
HTML blocks are rendered verbatim, unless HTML bleaching is enabled.
 * **Hard Rule**: A line consisting of 3+ `-` defines a hard rule.
 * **Table**: A `|`character at the beginning of a line begins a table row, consisting of table cells separated by `|` characters.
The cells are themselves blocks, and as such can contain newlines. The rows are terminated by a `|` followed by a newline.
By default the table has only a body, but if rows are separated by an alternating string of `+` and multiple `-` characters,
e.g. `+---+---+`, then every row above the separator will be in the header and every row below will be in the body.
Optionally the table may start and end with such a separator as well.

#### Inline Nodes

Inline nodes can generally contain a sequence of other inline nodes, but cannot contain nodes of the same type.
Despite the name, inline nodes can span multiple lines, e.g. to accomodate line length limits.

The following inline node types are supported:
 * **Plaintext**: The default inline type; any text not in another inline node is plaintext. Rendered verbatim.
 * **Italic**: Surrounding text with `*` *italicizes* it. Italic nodes can contain any other type of inline node.
 * **Bold**: Surrounding text with `**` **bolds** it. Bold nodes can contain any other type of inline node.
 * **Code**: Surrounding text with `\`` renders it as `code`. The content is rendered verbatim.
 * **Link**: A [link](#) is written as \[*text*\](*href*). The *text* portion can contain any other type of inline node.
The *href* portion is the link destination, and is parsed verbatim except that any literal `(` or `)` must be escaped.
 * **Image**: A image is written as \!\[*alt*\](*src*). The *alt* and *src* values are parsed verbatim except that any literal
`[` or `]` in the *alt* value and `(` or `)` in the *src* value must be escaped.
 * **Footnote Reference**: Writing \^\[*footnote-name*\] defines a footnote reference.
It is rendered as a superscript footnote number^[footnote-numbering], and links to the footnote named *footnote-name*
if it is present in the footer.

#### Footer

A Blogdown document may optionally include a footer after the body.
The footer consists of a sequence of *footnote definitions*, each of which begins on a new line with \~\[*footnote-name*\] and consists of an arbitrary sequence of blocks.
A footnote definition is only terminated by another footnote definition or the end of the document.

### Escaping

Any character (special or not) can be escaped with `\\`. For a literal backslash, use `\\\\`.
A backslash before a newline acts as a continuation character.

## Planned improvements
 * Better error messages on parse failures
 * Windows support
 * Document building & running tests
 * Comments

~[inline] Inlining CSS and JS is not recommended if you will be rendering multiple Blogdown documents on a single page, e.g. multiple blog posts on a blog.
Doing so will degrade network and browser performance slightly.

~[underline-parser-complexity] Supporting underlines for headers requires the parser to look-ahead arbitrarily far, resulting in quadratic time complexity.

~[footnote-numbering] Footnotes are auto-numbered in order of appearance, starting from 0 by default (this can be changed by passing the `--footnote-index-from` flag).
