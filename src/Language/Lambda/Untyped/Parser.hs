module Language.Lambda.Untyped.Parser
  ( parseExpr
  , expr
  , errorBundlePretty
  ) where

import Data.Functor
import Data.Void
import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char

import Language.Lambda.Untyped.AST

type Parser = Parsec Void String

var :: Parser AST
var = Var <$> (space *> some (alphaNumChar <|> char '\''))

app :: Parser (AST -> AST -> AST)
app = App <$ optional spaceChar

lam :: Parser AST -> Parser AST
lam expr = do
  space
  char '\\'
  space
  v <- some alphaNumChar
  space
  char '.'
  space
  Lam v <$> expr

subexpr :: Parser AST
subexpr = var <|> lam expr <|> char '(' *> expr <* char ')'

expr :: Parser AST
expr = makeExprParser subexpr [[InfixL app]]

parseExpr = parse (expr <* eof) ""
