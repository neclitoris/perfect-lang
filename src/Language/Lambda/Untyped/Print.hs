module Language.Lambda.Untyped.Print
  ( pretty
  ) where

import Data.Functor.Foldable
import Text.Printf

import Language.Lambda.Untyped.AST

pretty :: AST -> String
pretty = cata $ \case
  VarF v -> v
  AppF x y -> mconcat ["(", x, y, ")"]
  LamF v x -> mconcat ["(\\", v, ".", x, ")"]
