module Main where

import Data.Text.Lazy.IO qualified as T
import Prettyprinter.Render.Text

import Language.Lambda.Untyped.Eval
import Language.Lambda.Untyped.Parser
import Language.Lambda.Untyped.Print


main = do
  str <- getLine
  case parseExpr str of
    Right expr -> T.putStrLn $ showAST $ reduce expr
    Left err -> error $ errorBundlePretty err
