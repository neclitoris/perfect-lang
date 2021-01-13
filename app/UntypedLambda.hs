module Main where

import Language.Lambda.Untyped.Eval
import Language.Lambda.Untyped.Parser
import Language.Lambda.Untyped.Print


main = do
  str <- getLine
  case pretty . reduce <$> parseExpr str of
    Right str -> putStrLn str
    Left err -> error $ show err
