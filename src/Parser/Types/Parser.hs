module Parser.Types.Parser
  ( --
    -- * Parser types
    ErrorKind (..),
    Context (..),
    Error (..),
    Parser (..),
    Result,
    -- * Utilities
    extract,
    mkSource,
    -- * Tests only
    test_mkSrc,
    test_mkPos,
    -- * Internal only (hiding by Parser reimport)
    Pos (..),
    Src (..),
  )
where


import Control.Applicative


newtype Pos = Pos {unPos :: Int}
  deriving stock (Show)
  deriving newtype (Eq)

test_mkPos :: Int -> Pos
test_mkPos = Pos

newtype Context = Context {unContext :: String}
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

-- | An input source with position and text to be parsed
data Src = Src
  { srcPos :: !Pos,
    srcText :: !String
  }
  deriving stock (Show, Eq)

-- | Expose a smart constructor to build a Src
mkSource :: String -> Src
mkSource str = Src {srcPos = Pos 0, srcText = str}

test_mkSrc :: Int -> String -> Src
test_mkSrc pos = Src (Pos pos)

-- | Extract next character if available from source
-- and update character position in source
extract :: Src -> Maybe (Char, Src)
extract (Src _ []) = Nothing
extract (Src (Pos loc) (x : xs)) =
  Just
    ( x,
      Src (Pos $ loc + 1) xs
    )

-- | Make life easier with an alias
type Result a = Either Error (Src, a)

-- | A parser that can parse a Src and return an error or
-- the parsing result with remaining source
newtype Parser a = Parser
  { runParser :: Src -> Result a
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \src -> do
    -- Either monad handle failure
    (src', x) <- p src
    -- Use Tuple functor to apply f
    -- Same as: return (src', f x)
    return (fmap f (src', x))

instance Applicative Parser where
  -- pure here means Right
  pure x = Parser (\src -> pure (src, x))
  (Parser p1) <*> (Parser p2) = Parser $ \src -> do
    -- Get wrapped function from first parser
    (s1, f) <- p1 src
    -- Get wrapped value from second parser
    (s2, x) <- p2 s1
    -- Tuple functor to apply f to x inside Either monad
    return (fmap f (s2, x))

instance Monad Parser where
  (Parser p1) >>= f = Parser $ \src -> do
    -- Extract value in either monad
    (s1, x) <- p1 src

    -- Apply function and wrap it again
    runParser (f x) s1

instance Alternative Parser where
  empty :: Parser a
  -- Discard provide source and always returns (Left err)
  empty = Parser $ const (Left err)
    where
      -- Build error when empty
      err = Error (Pos 0) Empty (Context "Empty")

  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser p1) <|> (Parser p2) = Parser $ \src ->
    case p1 src of
      -- If first failed try the other one
      (Left _) -> p2 src
      -- If first is a success ignore other one
      result -> result

-- instance Alternative Parser where
--   empty :: Parser a
--   -- Discard provide source and always returns (Left err)
--   empty = Parser $ const (Left err)
--     where
--       -- Build error when empty
--       err = Error (Pos 0) Empty (Context "Empty Alternative")

--   (<|>) :: Parser a -> Parser a -> Parser a
--   (Parser p1) <|> (Parser p2) = Parser $ \src ->
--     case p1 src of
--       -- If first failed without consuming any char
--       -- try the other one else propagate the failure
--       failure@(Left (Error newPos _ _)) ->
--         if newPos == srcPos src
--           then p2 src
--           else failure
--       -- If first is a success ignore other one
--       result -> result

-- How ??
-- instance (Show a) => Show (Parser a) where
--     show (Parser p) = show (\src -> runParser p src)
