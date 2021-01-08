module Parser.Types.Markdown (
  -- $bnf
  Level (..),
  Paragraph (..),
  TextM (..),
  Link,
  Image,
) where

import Data.Text as T


newtype Level = Level {unLevel :: Int}
  deriving stock (Show)
  deriving newtype (Eq)

data Paragraph
  = Normal [TextM]
  | Heading Level [TextM]
  deriving stock (Eq, Show)

data TextM
  = Text T.Text
  | Emph TextM
  | Bold TextM
  | Strike TextM
  | Underscore TextM
  | Code TextM
  | Tag T.Text
  deriving stock (Show, Eq)

data Link = Link {linkName :: T.Text, linkURI :: T.Text} deriving stock (Show, Eq)
data Image = Image {imageAlt :: T.Text, imageURI :: T.Text} deriving stock (Show, Eq)


{- $bnf


__BNF used to implement combinators and Markdown AST__

@

  # TEXT
  textm        ::= bold | italic | strike | underscore | code | tag | text
  text         ::= (modifiersEsc | !(modifiers | escaped | linebreak | newline)+ )*
  bold         ::= "**" ( textm | text )+ "**"
  italic       ::= "*"  ( textm | text )+ "*"
  strike       ::= "~~" ( textm | text )+ "~~"
  underscore   ::= "__" ( textm | text )+ "__"
  code         ::= "`"  ( text )+ "`"
  tag          ::= "#"  ( text )+

  # SPECIAL CHARACTER
  modifiers    ::= "*" | "~" | "_" | "`" | "#"
  delimiters   ::= "(" | ")" | "[" | "]" | "|"
  modifiersEsc ::= "\\" modifiers

  # LINE BREAK
  linebreak    ::= "  " newline
  newline      ::= "\r\n" | "\n"

  # HEADERS
  header       ::= "#"+ " " {textm}+

  # LINK
  link         ::= "[" text "](" text ")"
@

__AST__ is implemented in `Parser.Types.Markdown`
and __combinators__ are implemented in `Parser.Combinators.Markdown`

-}
