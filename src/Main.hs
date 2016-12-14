{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Data.Int
import Text.Parsec

import False.Parser
import False.Interpreter

main :: IO ()
main = do
  conts <- getContents
  case parse parser "<console>" conts of
    Left msg -> do
      putStrLn $ "parser error: " ++ (show msg)
    Right ts -> do
      ret <- runEval ts
      case ret of
        Left msg -> putStrLn $ "runtime error: " ++ msg
        Right stack -> putStrLn $ show stack
  where parser :: FalseParser Int32 = parseFalse
