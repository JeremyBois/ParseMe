{- Implementation of Parser types and instances -}
module Parser.Types.Parser (
  -- ** Types
  ErrorKind (..),
  Context (..),
  Error (..),
  -- ParserT (..),
  Parser,
  Result,

  -- ** Utilities
  extract,
  mkSource,
  parse,
  runParser,
  evalParser,
  match,
  char,
  eof,

  -- ** Internal
  Pos (..),
  Src (..),
  test_mkSrc,
  test_mkPos,
) where

import Control.Applicative

import Control.Monad.Trans.State.Strict

import Data.Text as T

newtype Pos = Pos {unPos :: Int}
  deriving stock (Show)
  deriving newtype (Eq)

test_mkPos :: Int -> Pos
test_mkPos = Pos

newtype Context = Context {unContext :: T.Text}
  deriving stock (Show)
  deriving newtype (Eq)

data ErrorKind
  = UnexpectedEof
  | UnexpectedChar !(Char, Char)
  | MissmatchPredicate !Char
  | Empty
  deriving stock (Show, Eq)

-- | A Parsing error description (position and input)
data Error = Error !Pos !ErrorKind !Context
  deriving stock (Show, Eq)

-- Must implement Semigroup for Alternative constraint
-- Only keep last error for now
instance Semigroup Error where
  _ <> e2 = e2

-- Must implement Monoid for Alternative constraint
-- Reuse empty from previous implementation (without State)
instance Monoid Error where
  mempty = Error (Pos 0) Empty (Context "Empty")

-- | An input source with position and text to be parsed
data Src = Src
  { srcPos :: !Pos,
    srcText :: !T.Text
  }
  deriving stock (Show, Eq)

-- | Expose a smart constructor to build a Src
mkSource :: T.Text -> Src
mkSource str = Src {srcPos = Pos 0, srcText = str}

test_mkSrc :: Int -> T.Text -> Src
test_mkSrc pos = Src (Pos pos)

{- | Extract next character if available from source
 and update character position in source
-}
extract :: Src -> Maybe (Char, Src)
extract (Src (Pos loc) txt) =
  if T.null txt
    then Nothing
    else
      Just
        ( T.head txt,
          Src (Pos $ loc + 1) (T.tail txt)
        )

-- data PResult e a
--   = -- | Parser succeeded
--     Success a
--   | -- | Parser failed
--     Failed e

-- All are the same
-- newtype ParserT e a = ParserT {unParserT :: Src -> (Either e a, Src)}
-- newtype ParserT e s a = ParserT {unParserT :: s -> (Either e a, s)}
-- newtype ParserT e a = ParserT {unParserT :: State Src (Either e a)}
-- newtype ParserT e m a = ParserT {unParserT :: m (Either e a)}

{- | A specialized version of ExceptT (EitherT transformer) with and internal State
 to keep track of remaining text to be parsed.
 More general version could accept any monad in place of a State
 >>> newtype ParserT e m a = ParserT {unParserT :: m (Either e a)}
 State is used to avoid having to pass the global state manually:
 >>> newtype ParserT e s a = ParserT {unParserT :: s -> (Either e a, s)}
-}
newtype ParserT e s a = ParserT {unParserT :: State s (Either e a)}

instance Functor (ParserT e s) where
  -- Run the parser and map f over the result
  -- Go in State then in Either using respectively <$> and fmap
  fmap f pT = ParserT $ fmap f <$> unParserT pT

instance Applicative (ParserT e s) where
  -- Use already define applicative instance of State and Either
  pure val = ParserT $ pure $ Right val
  (ParserT pf) <*> (ParserT px) = ParserT $ do
    -- Try getting partial function
    fE <- pf
    case fE of
      (Left e) -> return $ Left e
      (Right f) -> do
        -- Try getting next function argument
        xE <- px
        case xE of
          (Left e) -> return $ Left e
          -- Both function and argument exists
          -- so call function with added argument
          (Right x) -> return $ Right $ f x

instance Monad (ParserT e s) where
  -- return = pure
  (ParserT px) >>= pf = ParserT $ do
    xE <- px
    case xE of
      (Left err) -> return $ Left err
      (Right x) -> unParserT $ pf x

instance (Monoid e) => Alternative (ParserT e s) where
  -- Error type should have an instance of Monoid
  -- to provided mempty
  empty = ParserT $ return (Left mempty)

  (ParserT p1) <|> (ParserT p2) = ParserT $ do
    -- Get previous State to be able to revert
    oldSrc <- get
    -- Try first parser
    p1Result <- p1
    case p1Result of
      -- First revert the global state ...
      -- ... then
      --    If p2 fails then combine both errors and wrap it in Left again
      --    If p2 succeeds then wrap result back in Right
      -- fmap allows to lift over State to reach the Either value
      -- Can be read as below
      -- (lift_over_State) (either (func_if_failure) (func_if_success)) (state_value)
      (Left err) -> put oldSrc >> fmap (either (Left . mappend err) Right) p2
      (Right x) -> return (Right x)


-- | Specialized version used in the library
type Parser = ParserT Error Src

-- | Alias for parser output
type Result a = Either Error a

runParser ::
  -- | Parser to run
  Parser a ->
  -- | Text to parse
  T.Text ->
  -- Parsing result
  (Result a, Src)
-- 1) Build a source from user text
-- 2) Extract State from transformer (function)
-- 3) Use our src as first argument for runState
runParser p str = runState (unParserT p) (mkSource str)

evalParser :: Parser a -> T.Text -> Result a
evalParser p = fst . runParser p

parse :: Parser a -> T.Text -> (Result a, Src)
parse = runParser

-- | Parse input character
char :: Char -> Parser Char
char askedChar = ParserT $ do
  -- Get State value
  src <- get

  case extract src of
    (Just (c, newSrc)) ->
      if c == askedChar
        then put newSrc >> (return . Right) c
        else
          (return . Left) $
            Error
              (srcPos src)
              (UnexpectedChar (askedChar, c))
              (Context "Specific Character")
    Nothing ->
      if askedChar == '\NUL'
        then (return . Right) askedChar
        else
          (return . Left) $
            Error (srcPos src) UnexpectedEof (Context "EOF")

match :: Context -> (Char -> Bool) -> Parser Char
match context f = ParserT $ do
  src <- get
  go src
  where
    go src@(extract -> Just (c, newSrc))
      | f c = put newSrc >> (return . Right) c
      | otherwise =
        (return . Left) $
          Error
            (srcPos src)
            (MissmatchPredicate c)
            context
    -- Nothing case
    go src =
      (return . Left) $
        Error (srcPos src) UnexpectedEof context

-- | Failed if not EOF else always succeed without moving the cursor position
eof :: Parser ()
eof = ParserT $ do
  (Src pos txt) <- get

  if T.null txt
    then -- EOF parsed
      (return . Right) ()
    else -- Still something to parse
      (return . Left) $ Error pos (UnexpectedChar ('\NUL', T.head txt)) (Context "Parse EOF")
