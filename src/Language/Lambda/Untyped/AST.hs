{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Lambda.Untyped.AST where

import Data.Deriving
import Data.Functor.Compose
import Data.Functor.Foldable
import Data.List
import GHC.Generics

data ASTF a
  = VarF String
  | AppF a a
  | LamF String a
  deriving (Eq, Show, Generic, Functor)

data MarkedF t = MarkedF
  { bound :: [String],
    free :: [String],
    term :: t
  }
  deriving (Show, Eq, Generic, Functor)

$(deriveEq1 ''ASTF)
$(deriveOrd1 ''ASTF)
$(deriveShow1 ''ASTF)
$(deriveFoldable ''ASTF)
$(deriveTraversable ''ASTF)

$(deriveEq1 ''MarkedF)
$(deriveOrd1 ''MarkedF)
$(deriveShow1 ''MarkedF)
$(deriveFoldable ''MarkedF)
$(deriveTraversable ''MarkedF)

type AST = Fix ASTF
type Marked a = Fix (Compose MarkedF a)
type MarkedAST = Marked ASTF

pattern Var x = Fix (VarF x)
pattern App f x = Fix (AppF f x)
pattern Lam v x = Fix (LamF v x)

pattern Marked b f t = Fix (Compose (MarkedF b f t))


mark :: AST -> MarkedAST
mark = cata collect
  where
    collect (VarF x) = Marked [] [x] (VarF x)
    collect (AppF x@(Marked b1 f1 _) y@(Marked b2 f2 _))
      = Marked (b1 `union` b2) (f1 `union` f2) (AppF x y)
    collect (LamF v x@(Marked b f _)) = Marked (b `union` [v]) (v `delete` f) (LamF v x)

unmark :: MarkedAST -> AST
unmark = cata (Fix . term . getCompose)

