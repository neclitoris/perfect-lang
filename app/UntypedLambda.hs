module Main where

import Language.Lambda.Untyped.Eval
import Language.Lambda.Untyped.Parser
import Language.Lambda.Untyped.Print


main = do
  str <- getLine
  case parseExpr str of
    Right expr -> putStrLn $ pretty $ reduce expr
    Left err -> error $ errorBundlePretty err
