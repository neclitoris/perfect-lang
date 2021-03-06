{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
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

data MarkedE t = MarkedE
  { bound :: [String],
    free :: [String],
    term :: t
  }
  deriving (Eq, Show, Generic, Functor)

$(deriveEq1 ''ASTF)
$(deriveOrd1 ''ASTF)
$(deriveShow1 ''ASTF)
$(deriveFoldable ''ASTF)
$(deriveTraversable ''ASTF)

$(deriveEq1 ''MarkedE)
$(deriveOrd1 ''MarkedE)
$(deriveShow1 ''MarkedE)
$(deriveFoldable ''MarkedE)
$(deriveTraversable ''MarkedE)

type AST = Fix ASTF

type MarkedF a = Compose MarkedE a

type Marked a = Fix (Compose MarkedE a)

type MarkedAST = Marked ASTF

pattern Var x = Fix (VarF x)

pattern App f x = Fix (AppF f x)

pattern Lam v x = Fix (LamF v x)

pattern MarkedF b f t = Compose (MarkedE b f t)

pattern Marked b f t = Fix (Compose (MarkedE b f t))

pattern VarM b f x = Marked b f (VarF x)

pattern AppM b f x y = Marked b f (AppF x y)

pattern LamM b f x y = Marked b f (LamF x y)

mark :: AST -> MarkedAST
mark = cata markStep

markStep :: ASTF MarkedAST -> MarkedAST
markStep (VarF x) = Marked [] [x] (VarF x)
markStep (AppF x@(Marked b1 f1 _) y@(Marked b2 f2 _)) =
  Marked (b1 `union` b2) (f1 `union` f2) (AppF x y)
markStep (LamF v x@(Marked b f _)) = Marked (b `union` [v]) (v `delete` f) (LamF v x)

unmark :: MarkedAST -> AST
unmark = cata (Fix . term . getCompose)

