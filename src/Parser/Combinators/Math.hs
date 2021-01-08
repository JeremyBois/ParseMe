module Parser.Combinators.Math (
  -- *** Math
  intP,
  exprP,
) where

import Parser.Combinators.Primitive
import Parser.Types (Parser)

--
-- Math combinators
--

{- | BNF for a simple math expression with natural numbers
 expr ::= term {op term}*
 term ::= nat | '(' expr ')'
 op ::= '+' | '-'
 nat ::= {digit}+
 digit ::= '0' | '1' | ... | '9'
-}
data Expr = Const Int | Bin Expr Op Expr deriving stock (Eq, Show)

data Op = Plus | Minus deriving stock (Eq, Show)

{- | Recursively build an expression
 from an fully parenthesised expressions
-}
exprP :: Parser Expr
exprP = token (termP >>= rest)
  where
    rest expr = (Bin expr <$> opP <*> termP) <|> pure expr

termP :: Parser Expr
termP = token (constantP <|> enclosed exprP)
  where
    enclosed = between (symbol "(") (symbol ")")

opP :: Parser Op
opP = plus <|> minus
  where
    plus = Plus <$ symbol "+"
    minus = Minus <$ symbol "-"

constantP :: Parser Expr
constantP = Const <$> intP

intP :: Parser Int
intP = read <$> some digitP
