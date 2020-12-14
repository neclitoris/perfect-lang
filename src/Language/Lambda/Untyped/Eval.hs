module Language.Lambda.Untyped.Eval where

import Data.Functor.Foldable
import Language.Lambda.Untyped.AST

data Alpha = Alpha String AST deriving Show

-- TODO
applyAlpha :: Alpha -> AST -> AST
applyAlpha (Alpha var t') = cata app
  where
    app (VarF v)
      | v == var = t'
      | otherwise = Var v
    app (AppF f x) = App f x
    app (LamF v x) = Lam v x

reduce :: AST -> AST
reduce = _a
