# Introduction

This repository contains experiments in using Prolog to write parsers.

The motivation for writing this is the difficulties in writing parsers for lightweight markup languages, such as Markdown and AsciiDoc.
These languages generally evolve organically and parsers are implemented manually without using the parsing tools generally used to write parsers for programming languages.
These manual parsers often have limitations, such as not generating a complete AST that describes the structure of a file accurately.
These ASTs would be useful to implement tooling.
For example, an AST for a Markdown document that includes accurate information about where inline code and code blocks begin and end, would make it simpler to implement a spell checker that skips over code.

Many attempts to write parsers for lightweight markup languages have run into hurdles due to the structure of those languages.
For example:

* https://www.tweag.io/blog/2021-06-15-asciidoc-haskell-pandoc/ describes many of the complexities in parsing AsciiDoc.
* https://github.com/jgm/djot is a lightweight markup language based on Markdown created by John MacFarlane, the author of Pandoc.
  Djot tries to avoid the points that make Markdown hard to parse.
  Even with a structure friendlier to parsing, Djot still uses a hand rolled parser (but produces a full AST).

Prolog was originally designed to parse natural language and might be uniquely suited to write parsers for these languages.
Prolog includes support for "definite clause grammars" (DCGs), a declarative way of expressing grammar rules.
DCGs can use Prolog features to express more complex grammar rules than what common parser tools allow.

https://github.com/rla/prolog-markdown is a Markdown parser written in Prolog.
However, for efficiency, this parser requires many optimizations that make the code harder to understand.

https://github.com/mthom/scryer-prolog/ is a recent Prolog implementation that includes [pioneering optimizations in string handling](https://github.com/mthom/scryer-prolog/#strings-and-partial-strings).
Some examples in this repo use Scryer Prolog, but some other require SWI-Prolog, which has more mature tooling and, for example, makes it easier to write unit tests.

Additionally, this repo tries to explain how to write parsers in Prolog for people who have no experience with Prolog, or with no parsing experience.

# Installing Scryer Prolog

Scryer Prolog is written in Rust.
Follow [the instructions to install Rust](https://rustup.rs/).
Then install the latest development code of Scryer Prolog:

```
$ cargo install --git https://github.com/mthom/scryer-prolog.git
```

# Examples

* [simple.pro](simple.pro) is a proof of concept that demonstrates that Prolog can be used to parse and generate ASTs.
  The program parses files following a very simple grammar used in many DCG tutorials, and outputs a JSON AST.

    The JSON AST does not contain position information, but it is a "complete" AST.
    The AST includes all text in the parsed text, so position information can be calculated from the AST.

* [lm.pro](lm.pro) is a proof of concept of parsing lightweight markup languages.

* [asciidoc_poc.pro](asciidoc_poc.pro) a parser that handles constrained and unconstrained formatting in AsciiDoc.
