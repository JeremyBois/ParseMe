module Parser.Combinators.Markdown (
  -- *** Primitives
  tagP,
  modifiersP,
  delimitersP,
  modifiersEscP,
  linebreakP,

  -- *** Data
  textP,
) where

import Data.Char (isLetter)

import Data.Text as T

import Parser.Combinators.Primitive
import Parser.Types (
  -- Parser
  Context (..),
  Parser,
  -- Markdown
  TextM (..),
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
notTextCharacters = modifiers <> delimiters <> newline <> T.singleton '\\'

isElem :: Char -> Text -> Bool
isElem c = T.any (== c)

--
-- Markdown combinators
--

{- | Parser for a markdown modifier (see BNF modifiers).

==== __Examples__

>>> runParser (some modifiersP) (mkSource "*_`~#")
Right (Src {srcPos = Pos {unPos = 6}, srcText = ""},"*_`~#")
-}
modifiersP :: Parser Char
modifiersP = match (Context "Modifiers") (`isElem` modifiers)

{- | Parser for a delimiter (see BNF delimiters)

==== __Examples__

>>> runParser (some delimitersP) (mkSource "[]()|")
Right (Src {srcPos = Pos {unPos = 5}, srcText = ""},"[]()|")
-}
delimitersP :: Parser Char
delimitersP = match (Context "Delimiters") (`isElem` delimiters)

{- | Parser for an escaped modifier (see BNF modifiersEsc)

==== __Examples__

>>> runParser (some modifiersEscP) (mkSource "\\*\\_\\`\\~\\#")
Right (Src {srcPos = Pos {unPos = 10}, srcText = ""},"*_`~#")
>>> runParser (some modifiersEscP) (mkSource "*_`~#")
Left (Error (Pos {unPos = 0}) (UnexpectedChar ('\\','*')) (Context {unContext = "Specific Character"}))
-}
modifiersEscP :: Parser Char
modifiersEscP = char '\\' *> modifiersP

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

-- | Parser for markdown text (see BNF text)
textP :: Parser TextM
textP =
  Text . T.pack . mconcat
    <$> many
      ( some modifiersEscP
          <|> some
            ( match
                (Context "Valid text")
                (not . (`isElem` notTextCharacters))
            )
      )


tagP :: Parser TextM
tagP = Tag . T.pack <$> (char '#' *> some (match (Context "Word") isLetter) <* spaceP)

-- boldP :: Parser T.Text
-- boldP = between (symbol "**") (symbol "**") (some normalCharP)

-- italicP :: Parser T.Text
-- italicP = between (symbol "*") (symbol "*") (some normalCharP)

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
