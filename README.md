# ParseMe

An exploratory project of [**parser combinators**](https://en.wikipedia.org/wiki/Parser_combinator) to get more used to the following tools:
  - Functor
  - Applicative
  - Monad
  - Alternative
  - Tranformer
  - Error handling
  - Haddock documentation generation

## Playing
If you want to test it, simply use `ghci` with `cabal`.
```bash
cabal repl ParseMe
```

Below a simple example illustrating how to use it. This example try to parse a markdown file composed of two lines, one with a header, one with text with some formatting.
```haskell
-- Parse two markdown lines (result and buffer state)
>>> runParser (some paragraphP) "## Heading \n Text **Bold**"
Right [Heading (Level {unLevel = 2}) [Text "Heading "],Line [Text " Text ",Bold [Text "Bold"]]],Src {srcPos = Pos {unPos = 26}, srcText = ""})

-- Parse two markdown lines (result only)
>>> evalParser (some paragraphP) "## Heading \n Text **Bold**"
Right [Heading (Level {unLevel = 2}) [Text "Heading "],Line [Text " Text ",Bold [Text "Bold"]]]
```

In case of parsing error, context and failure detail are given. Below an example where we want to parse a bold text (`**text**`) but there is a missing `*`.
```haskell
-- Full output (failure result and buffer state)
>>> runParser boldP "*Bold**"
(Left (Error (Pos {unPos = 1}) (UnexpectedChar ('*','B')) (Context {unContext = "Specific Character"})),Src {srcPos = Pos {unPos = 1}, srcText = "Bold**"})

-- Get a less verbose output by discarding buffer state (failure result only)
>>> evalParser boldP "*Bold**"
Left (Error (Pos {unPos = 1}) (UnexpectedChar ('*','B')) (Context {unContext = "Specific Character"}))

-- Get failure information in a more human friendly syntax
>>> prettyResult $ evalParser boldP "*Bold**"
"ERROR :: Expected <*> but get <B> at Pos {unPos = 1} (UnexpectedChar (*, B) - Specific Character)."
```

Note that **nested expressions** and **Unicode** are also supported out of the box.
```haskell
>>> printResult $ evalParser paragraphP "**😎 with some `smiley` *😂* *🔥* **"
Line [Bold [Text "😎 with some ",InlineCode [Text "smiley"],Text " ",Emph [Text "😂"],Text " ",Emph [Text "🔥"],Text " "]]
```


## Documentation
Just run the following to build the documentation and you are done !
```bash
cabal haddock
```


## Description
### Parser
A parser is a program that take input data (JSON, raw text, ...) and build a hierarchical structure like an Abstract Syntax Tree (AST) following a set of rules that can be defined using a common notation known as Backus-Naur Form (BNF).
For example for a simple mathematical expression using only natural numbers the BNF could be written as below:
```bnf
{- | BNF for a simple math expression with natural numbers
 expr ::= term {op term}*
 term ::= nat | '(' expr ')'
 op ::= '+' | '-'
 nat ::= {digit}+
 digit ::= '0' | '1' | ... | '9'
-}
```

### Parser combinator
A parser combinator is a parser built using a combination of multiple other parsers allowing to recursively parse an input. This approach allows to create primitives that can parse only a specific form and combine them to create complex parser without sacrificing modularity, readability and maintenability.

As an example, how can we parse the following markdown text ?

```markdown
Hello **dear *Thomas* **
```

Using the combinator approach we can split it in multiple small parts:
  - `textNormalParser` a parser for `text`
  - `textBoldParser` a parser for `**text**`
  - `textEmphParser` a parser for `*text*`

Then, to be able to parse the whole line we only have to combine them (here `<|>` is used as a combine operator because its the syntax chosen in the `Alternative` typeclass).
Our complete parser can then be written only using our small combinators as follow:
```haskell
completeParser = textNormalParser <|> textBoldParser <|> textEmphParser
```

**Now, to understand how the whole thing is made possible, just take a look at the code !**


### Library structure
In this library I implement a fully working combinator parser with error generation when parsing failed. See [Documentation](#documentation) for API description and example of how to use it.

In a few words the general lib structure is as follow:
  - Types
    - Markdown - AST definition using BNF notation
    - Parser -  Implementation and core primitive (char, eof, match, runParser, ...)
  - Combinators
    - Markdown - Combinators to parse markdown text using Applicative style
    - Math - Math combinators to parse simple mathematical expression
    - Other - Combinators for phone number using Monad style
    - Primitive - Building blocs used to build specialized combinators (choice, between, manyTill, ...)


