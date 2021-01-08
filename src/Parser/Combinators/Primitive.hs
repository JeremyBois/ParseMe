{-# LANGUAGE LambdaCase #-}

module Parser.Combinators.Primitive
  ( --
    -- * Reimports
    some,
    many,
    (<|>),
    -- * Base
    match,
    char,
    token,
    symbol,
    eof,
    Parser.Combinators.Primitive.any,
    -- * Alternative
    choice,
    between,
    sepByMany,
    sepBySome,
    -- * Convenience
    digitP,
    stringP,
    spaceP,
  )
where

import Control.Applicative (some, many, (<|>))
import Data.Char (isDigit, isSpace)

import Data.List.NonEmpty (NonEmpty)

import Parser.Types

--
--  Primitive combinators
--

-- | Parse next character if it matches a predicate.
match :: Context -> (Char -> Bool) -> Parser Char
match context f = Parser go
  where
    go src@(extract -> Just (foundChar, newSrc))
      | f foundChar = Right (newSrc, foundChar)
      | otherwise =
        Left $
          Error
            (srcPos src)
            (MissmatchPredicate foundChar)
            context
    -- Nothing case
    go src =
      Left $
        Error (srcPos src) UnexpectedEof context

-- | Parse input character
char :: Char -> Parser Char
char askedChar = Parser go
  where
    -- Take avantage of ViewPatterns to pattern on functions
    -- Less verbose version of case expression
    -- go = \src -> case extract src of
    --   Just (foundChar, newSrc) -> undefined
    --   Nothing                -> undefined
    go src@(extract -> Just (foundChar, newSrc))
      | foundChar == askedChar = Right (newSrc, foundChar)
      | otherwise =
        Left $
          Error
            (srcPos src)
            (UnexpectedChar (askedChar, foundChar))
            (Context "Specific Character")
    -- Nothing case
    go src =
      if askedChar == '\NUL'
        then Right (src, askedChar)
        else
          Left $
            Error (srcPos src) UnexpectedEof (Context "EOF")

-- | Transform a parser to also consume any leading space.
token :: Parser a -> Parser a
token p = many spaceP *> p

-- | Parse input string (first consume any leading space).
symbol :: String -> Parser String
symbol = token . stringP

-- | A parser that always parse successfuly expect for eof
any :: Parser Char
any = match (Context "Any Character") (const True)

{- | Select first working parser from a non empty list of choice

==== __Examples__

>>> import Data.List.NonEmpty (fromList)
>>> runParser (choice (fromList [char 'a', char 'b'])) (mkSource "abc")
Right (Src {srcPos = Pos {unPos = 1}, srcText = "bc"},'a')
-}
choice :: NonEmpty (Parser a) -> Parser a
choice = foldr1 (<|>)

{- | Parse a value enclosed between two matching parsers
First two parsers only consume the source respectively
before and after parsed data by last argument parser.

==== __Examples__

>>> runParser (between spaceP eof boolP) (mkSource " False")
Right (Src {srcPos = Pos {unPos = 6}, srcText = ""},False)

>>> runParser (between spaceP eof boolP) (mkSource "False")
Right (Src {srcPos = Pos {unPos = 6}, srcText = ""},False)
Left (Error (Pos {unPos = 0}) (MissmatchPredicate 'F') (Context {unContext = "Space"}))
-}
between ::
  -- | Parser used to consume characters before value of interest
  Parser b ->
  -- | Parsed used to consume characters after value of interest
  Parser c ->
  -- | Parser used to extract value of interest
  Parser a ->
  -- | Output a parser that can parse a value enclosed
  Parser a
between b a p = b *> p <* a

-- | Failed if not EOF else always succeed without moving the cursor position
eof :: Parser ()
eof = Parser $ \src -> case src of
  -- EOF parsed
  (Src _ []) -> Right (src, ())
  -- Still something to parse
  (Src pos (x : _)) -> Left $ Error pos (UnexpectedChar ('\NUL', x)) (Context "Parse EOF")

{- | Parse ZERO or more element separated with a specific separator

==== __Examples__

>>> runParser ((token digitP) `sepByMany` (token $ char ',')) (mkSource "2, 3")
Right (Src {srcPos = Pos {unPos = 4}, srcText = ""},"23")

>>> runParser ((token digitP) `sepByMany` (token $ char ',')) (mkSource "")
Right (Src {srcPos = Pos {unPos = 0}, srcText = ""},"")
-}
sepByMany ::
  Parser b ->
  Parser a ->
  Parser [b]
sepByMany elementP sepP = sepBySome elementP sepP <|> pure []

{- | Parse ONE or more element separated with a specific separator

==== __Examples__

>>> runParser ((token digitP) `sepByMany` (token $ char ',')) (mkSource "2, 3")
Right (Src {srcPos = Pos {unPos = 4}, srcText = ""},"23")

>>> runParser ((token digitP) `sepByMany` (token $ char ',')) (mkSource "")
Left (Error (Pos {unPos = 0}) UnexpectedEof (Context {unContext = "Digit"}))
-}
sepBySome ::
  -- | Parser used to parse elements
  Parser b ->
  -- | Parser used to parse separators
  Parser a ->
  Parser [b]
sepBySome elementP sepP = (:) <$> elementP <*> many (sepP *> elementP)

--
-- Convenience
--

-- | Parse next character is its a space else failed
spaceP :: Parser Char
spaceP = match (Context "Space") isSpace

-- | Parse next character if its a digit else failed
digitP :: Parser Char
digitP = match (Context "Digit") isDigit

-- | Parser all character of input string if all match else failed
stringP :: String -> Parser String
stringP = traverse (\c -> match (Context "String") (== c))


--
-- Tests
--

-- anyP :: Parser Char
-- anyP = Parser $ \case
--   -- EOF occurs
--   (Src pos []) ->
--     Left $ Error pos UnexpectedEof (Context "EOF")
--   -- Parse succeed
--   (Src (Pos loc) (x : xs)) ->
--     Right (Src {srcPos = Pos (loc + 1), srcText = xs}, x)

-- Cannot be done without a source in error part
-- tryP :: Parser a -> Parser a
-- tryP p = Parser $ \src -> case runParser p src of
--   Left (Error pos err context) -> Left (Error (Pos (((1 -) . unPos) pos)) err context)
--   result -> result

-- match :: Context -> (Char -> Bool) -> Parser Char
-- match context match = tryP $ do
--   c <- anyP
--   if match c
--     then pure c
--     else
--       Parser
--         (\src -> Left (Error (srcPos src) (MissmatchPredicate c) context))

-- char :: Char -> Parser Char
-- char asked = tryP $ do
--   found <- anyP
--   if found == asked
--     then pure found
--     else
--       Parser
--         ( \src ->
--             Left
--               ( Error
--                   (srcPos src)
--                   (UnexpectedChar (asked, found))
--                   (Context "Specific Character")
--               )
--         )
