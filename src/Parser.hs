-- | Module containing parser data
module Parser (
  -- * Types
  module Types,

  -- * Combinators
  -- $combinator
  module Y,

  -- * Debug
  -- $debug
  Parser.print,
) where

import Parser.Combinators as Y
import Parser.Types as Types hiding (Pos (..), Src (..))

print :: (Show a) => Result a -> String
print (Left err) = printError err
print (Right res) = show res

printError :: Error -> String
printError (Error pos err context) = case err of
  UnexpectedEof ->
    mconcat
      [ "ERROR :: Get <",
        "EOF",
        "> at ",
        show pos,
        " (",
        show err,
        " - ",
        unContext context,
        ")."
      ]
  (UnexpectedChar (asked, found)) ->
    mconcat
      [ "ERROR :: Expected <",
        [asked],
        "> but get <",
        [found],
        "> at ",
        show pos,
        " (",
        show err,
        " - ",
        unContext context,
        ")."
      ]
  (MissmatchPredicate found) ->
    mconcat
      [ "ERROR :: Get <",
        [found],
        "> at ",
        show pos,
        " (",
        show err,
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
