module Language.Lambda.Untyped.Print where

import Language.Lambda.Untyped.AST
import Text.Printf

pretty :: AST -> String
pretty (Var v) = v
pretty (App x@Lam{} y@App{}) = printf "(%s) (%s)" (pretty x) (pretty y)
pretty (App x@Lam{} y) = printf "(%s) %s" (pretty x) (pretty y)
pretty (App x y@App{}) = printf "%s (%s)" (pretty x) (pretty y)
pretty (App x y) = printf "%s %s" (pretty x) (pretty y)
pretty (Lam v x) = printf "\\%s. %s" v (pretty x)
