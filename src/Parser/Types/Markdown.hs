module Parser.Types.Markdown (
  -- $bnf
  Paragraph (..),
  Style (..),
  Link,
  Image,
  -- * Smart constructors
  -- Level,
  -- mkLevel,
) where

import Data.Text as T


newtype Level = Level {unLevel :: Int}
  deriving stock (Show)
  deriving newtype (Eq)

-- mkLevel :: String -> Level
-- mkLevel = Level . Prelude.length

data Paragraph
  = Normal [Style]
  | Heading Level [Style]
  deriving stock (Eq, Show)

data Style
  = Text T.Text
  | Emph [Style]
  | Bold [Style]
  | Strike [Style]
  | Underscore [Style]
  | InlineCode [Style]
  | Tag T.Text
  deriving stock (Show, Eq)

data Link = Link {linkName :: T.Text, linkURI :: T.Text} deriving stock (Show, Eq)
data Image = Image {imageAlt :: T.Text, imageURI :: T.Text} deriving stock (Show, Eq)


{- $bnf


__BNF used to implement combinators and Markdown AST__

@

  # TEXT
  style        ::= bold | emphasize | strike | underscore | code | tag | text
  text         ::= (modifiersEsc | !(modifiers | escaped | linebreak | newline)+ )+
  bold         ::= "**" ( style )+ "**"
  emphasize    ::= "*"  ( style )+ "*"
  strike       ::= "~~" ( style )+ "~~"
  underscore   ::= "__" ( style )+ "__"
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
  header       ::= "#"+ " " {style}+

  # LINK
  link         ::= "[" text "](" text ")"
@

__AST__ is implemented in `Parser.Types.Markdown`
and __combinators__ are implemented in `Parser.Combinators.Markdown`

-}
