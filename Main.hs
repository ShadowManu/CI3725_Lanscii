-- File: Main.hs
-- Description: lanscii language core interpreter program
-- Authors:
--     Manuel Pacheco - 10-10524
--     Nicolas Mañan - 06-39883

module Main (main) where

import System.Environment
import Alex
import MyHappy

main = do
  args <- getArgs
  s <- readFile . head $ args
  mapM_ (putStrLn . display) . tokenize $ s
