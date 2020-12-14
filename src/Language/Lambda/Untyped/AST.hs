{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Lambda.Untyped.AST where

import Data.Functor.Foldable
import GHC.Generics

data AST
  = Var String
  | App AST AST
  | Lam String AST
  deriving (Eq, Show, Generic)

data ASTF a
  = VarF String
  | AppF a a
  | LamF String a
  deriving (Eq, Show, Generic, Functor)

type instance Base AST = ASTF

instance Recursive AST

instance Corecursive AST
