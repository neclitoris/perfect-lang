module Language.Lambda.Untyped.Print
  ( pretty ) where

import Data.Functor.Foldable
import Language.Lambda.Untyped.AST
import Text.Printf

pretty :: AST -> String
pretty = cata $ \case
  VarF v -> v
  AppF x y -> printf "%s %s" x y
  LamF v x -> printf "\\%s. %s" v x
