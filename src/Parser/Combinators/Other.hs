module Parser.Combinators.Other (
  -- *** Phone numbers
  areaCodeP,
  phoneNumberP,

  -- *** Other
  boolP,
) where

import Control.Monad (replicateM)

import Data.Char (isDigit)

import Parser.Combinators.Primitive
import Parser.Types.Parser (Context (..), Parser)

--
-- Phone numbers
--

areaCodeP :: Parser String
areaCodeP = do
  -- Prefer _ <- to allow the parser to rewrite it in an applicative way using ApplicativeDo
  -- https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Applicative.html#v:-42--62-

  -- Consumes `(` if any
  _ <- many (char '(')

  -- Consume `00` or `+` if any
  _ <-
    replicateM 2 (char '0')
      <|> some (char '+')

  -- Get area code
  digits <- replicateM 2 digitP

  -- Consumes `)` if any
  _ <- many $ char ')'

  return digits

internationalPhoneNumberP :: Parser String
internationalPhoneNumberP = do
  _ <- areaCodeP

  _ <- many (char '0')

  digits <- replicateM 9 (token digitP)

  return $ '0' : digits

localPhoneNumberP :: Parser String
localPhoneNumberP = replicateM 10 (token digitP)

phoneNumberP :: Parser String
phoneNumberP =
  (internationalPhoneNumberP <|> localPhoneNumberP)
    <* ( some (match (Context "Not a digit") (not . isDigit))
          <|> ("" <$ eof)
       )

--
-- Others combinators
--

-- | Parse true/True or false/False as bool else failed
boolP :: Parser Bool
boolP = tryTrue <|> tryFalse
  where
    -- (<$) :: a -> Parser b -> Parser a
    -- Same as fmap . const
    -- Similar to replace wrapped value with
    -- first argument (here True or False)
    tryTrue = True <$ (stringP "True" <|> stringP "true")
    tryFalse = False <$ (stringP "False" <|> stringP "false")

-- -- | *> - Sequence actions, discarding the value of the first argument
-- -- <* - Sequence actions, discarding the value of the second argument
-- quoteP :: Parser String
-- quoteP = char '\"' *> many normalCharP <* char '\"'

-- normalCharP :: Parser Char
-- normalCharP = match (Context "Normal Character") ((&&) <$> (/= '"') <*> (/= '\n'))
