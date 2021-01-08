-- | Module containing parser data
module Parser (
  -- * Types
  module Types,

  -- * Combinators
  -- $combinator
  module Y,

  -- * Debug
  -- $debug
  pretty,
  Parser.print,
) where

import Data.Text as T
import Data.Text.IO as TIO

import Parser.Combinators as Y
import Parser.Types as Types hiding (Pos (..), Src (..))

print :: (Show a) => Result a -> IO ()
print s = do
  TIO.putStrLn $ pretty s

pretty :: (Show a) => Result a -> T.Text
pretty (Left err) = prettyError err
pretty (Right res) = T.pack $ show res -- @TODO Add pretty version

prettyError :: Error -> T.Text
prettyError (Error pos err context) = case err of
  UnexpectedEof ->
    mconcat
      [ "ERROR :: Get <",
        "EOF",
        "> at ",
        (T.pack . show) pos,
        " (",
        (T.pack . show) err,
        " - ",
        unContext context,
        ")."
      ]
  (UnexpectedChar (asked, found)) ->
    mconcat
      [ "ERROR :: Expected <",
        T.singleton asked,
        "> but get <",
        T.singleton found,
        "> at ",
        (T.pack . show) pos,
        " (",
        "UnexpectedChar (",
        T.singleton asked,
        ", ",
        T.singleton found,
        ")",
        " - ",
        unContext context,
        ")."
      ]
  (MissmatchPredicate found) ->
    mconcat
      [ "ERROR :: Get <",
        T.singleton found,
        "> at ",
        (T.pack . show) pos,
        " (",
        (T.pack . show) err,
        " - ",
        unContext context,
        ")."
      ]
  Empty -> "Empty"
--
-- Descriptions
--

{- $type
Module containing types for the Parser and Markdown AST.

We hide `Src` and `Pos` that are only needed internally.
-}

{- $combinator
Module containing all combinators.
-}

{- $Debug
Helper function to pretty print a Parser result to get more information about a failure
or a success
-}
