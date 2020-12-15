module Language.Lambda.Untyped.Parser where

import Data.Functor
import Language.Lambda.Untyped.AST
import Text.Parsec

type Parser = Parsec String ()

var :: Parser AST
var = Var <$> (spaces *> many1 (alphaNum <|> char '\''))

app :: Parser (AST -> AST -> AST)
app = optional space $> App

lam :: Parser AST -> Parser AST
lam expr = do
  spaces
  char '\\'
  spaces
  v <- many1 alphaNum
  spaces
  char '.'
  spaces
  Lam v <$> expr

subexpr :: Parser AST
subexpr = var <|> lam expr <|> char '(' *> expr <* char ')'

expr :: Parser AST
expr = chainl1 subexpr app

parse = Text.Parsec.parse (expr <* eof) ""
