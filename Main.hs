-- File: Main.hs
-- Description: lanscii language core interpreter program
-- Authors:
--     Manuel Pacheco - 10-10524
--     Nicolas Mañan - 06-39883

module Main (main) where

import System.Environment
import Alex
import Happy
import Display

main :: IO ()
main = do
  args <- getArgs
  s <- readFile . head $ args
  let tokens = tokenize s
      tree = happyParser tokens
  if (any (noParseToken) tokens)
    then mapM_ (putStrLn . display) tokens
    else mapM_ putStrLn . pDisplay $ tree
