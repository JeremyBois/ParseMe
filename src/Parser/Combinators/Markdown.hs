{-# LANGUAGE TupleSections #-}

module Parser.Combinators.Markdown (
  -- *** Primitives
  tagP,
  modifiersCharP,
  delimitersP,
  modifiersEscP,
  linebreakP,

  -- *** Data
  textP,
  boldP,
  emphP,
  inlineCodeP,
  styleP,
) where

import Data.Char (isLetter)

import Data.Text as T

import Parser.Combinators.Primitive
import Parser.Types (
  -- Parser
  Context (..),
  Parser,
  -- Markdown
  Style (..),
  -- Level,
  -- mkLevel,
 )

--
-- Constants
--

modifiers :: T.Text
modifiers = "*_`~#"

delimiters :: T.Text
delimiters = "[]()|"

newline :: T.Text
newline = "\n"

notTextCharacters :: T.Text
notTextCharacters = modifiers <> delimiters <> newline <> T.pack ['\\', '\00']

isElem :: Char -> Text -> Bool
isElem c = T.any (== c)

--
-- Markdown combinators
--

{- | Parser for a markdown modifier (see BNF modifiers).

==== __Examples__

>>> runParser (some modifiersCharP) (mkSource "*_`~#")
Right (Src {srcPos = Pos {unPos = 6}, srcText = ""},"*_`~#")
-}
modifiersCharP :: Parser Char
modifiersCharP = match (Context "Modifiers Characters") (`isElem` modifiers)

{- | Parser for a delimiter (see BNF delimiters)

==== __Examples__

>>> runParser (some delimitersP) (mkSource "[]()|")
Right (Src {srcPos = Pos {unPos = 5}, srcText = ""},"[]()|")
-}
delimitersP :: Parser Char
delimitersP = match (Context "Delimiters Characters") (`isElem` delimiters)

{- | Parser for an escaped modifier (see BNF modifiersEsc)

==== __Examples__

>>> runParser (some modifiersEscP) (mkSource "\\*\\_\\`\\~\\#")
Right (Src {srcPos = Pos {unPos = 10}, srcText = ""},"*_`~#")
>>> runParser (some modifiersEscP) (mkSource "*_`~#")
Left (Error (Pos {unPos = 0}) (UnexpectedChar ('\\','*')) (Context {unContext = "Specific Character"}))
-}
modifiersEscP :: Parser Char
modifiersEscP = char '\\' *> modifiersCharP

{- | Parser for an end of line (see BNF linebreak).

==== __Examples__

>>> runParser (some linebreakP) (mkSource "   \n")
Right (Src {srcPos = Pos {unPos = 4}, srcText = ""},"\n")
-}
linebreakP :: Parser Char
linebreakP = many spaceP' *> newlineP -- spaceP' consumes '\t'

-- | Parser for a newline (see BNF newline)
newlineP :: Parser Char
newlineP = char '\n'

--
-- Styles
--

-- | Parser for markdown text (see BNF text)
textP :: Parser Style
textP =
  Text . T.pack . mconcat
    <$> some
      ( some modifiersEscP
          <|> some
            ( match
                (Context "Valid text")
                (not . (`isElem` notTextCharacters))
            )
      )

styleParserBuilder :: Parser a -> Parser b -> Parser [Style]
styleParserBuilder before after = before *> someTill styleP after

-- | Parser for markdown bold text (see BNF bold)
boldP :: Parser Style
boldP =
  Bold
    <$> styleParserBuilder
      (char '*' *> char '*')
      (char '*' *> char '*')

-- | Parser for markdown emphasized text (see BNF emphasize)
emphP :: Parser Style
emphP =
  Emph
    <$> styleParserBuilder
      (char '*')
      (char '*')

-- | Parser for markdown inlined code (see BNF inlineCode)
inlineCodeP :: Parser Style
inlineCodeP =
  InlineCode
    <$> styleParserBuilder
      (char '`')
      (char '`')

-- | Parser for markdown striked text (see BNF strike)
strikeP :: Parser Style
strikeP =
  Strike
    <$> styleParserBuilder
      (char '~' *> char '~')
      (char '~' *> char '~')

-- | Parser for markdown underscore text (see BNF underscore)
underscoreP :: Parser Style
underscoreP =
  Underscore
    <$> styleParserBuilder
      (char '_' *> char '_')
      (char '_' *> char '_')

-- | Parser for markdown tagged text (see BNF tag)
tagP :: Parser Style
tagP = Tag . T.pack <$> (char '#' *> some (match (Context "Word") isLetter) <* spaceP)

{- Parser for any style (See BNF style)

==== __Examples__

>>> runParser styleP (mkSource "**b*be#tag be*b**")
Right (Src {srcPos = Pos {unPos = 17}, srcText = ""},Bold [Text "b",Emph [Text "be",Tag "tag",Text "be"],Text "b"])
>>> runParser (some styleP) (mkSource "text__u__~~s~~")
Right (Src {srcPos = Pos {unPos = 14}, srcText = ""},[Text "text",Underscore [Text "u"],Strike [Text "s"]])
-}
styleP :: Parser Style
styleP =
  textP
    <|> tagP
    <|> boldP
    <|> emphP
    <|> inlineCodeP
    <|> strikeP
    <|> underscoreP


-- --
-- -- Heading
-- --

-- levelP :: Parser Level
-- levelP = mkLevel <$> someTill (char '#') (char ' ')




-- linkP :: Parser (T.Text, T.Text)
-- linkP = do
--   n <- name (some $ charDifferentFromP (== ']'))
--   l <- link (some $ charDifferentFromP (== ')'))
--   return (n, l)
--   where
--     name = between (symbol "[") (symbol "]")
--     link = between (symbol "(") (symbol ")")

-- charDifferentFromP :: (Char -> Bool) -> Parser Char
-- charDifferentFromP f = match (Context "Character different from predicate") (not . f)
