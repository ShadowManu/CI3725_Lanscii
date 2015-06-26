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

import qualified SymbolTable as ST
import qualified SymbolChecking as SC

main :: IO ()
main = do
  args <- getArgs
  s <- readFile . head $ args
  let tokens = tokenize s
      tree = happyParser tokens
  -- If Lexer Error
  if any noParseToken tokens
    then mapM_ (putStrLn . display) tokens
    -- Parser Error is handled with error haskell routines
    -- Symbol Table analysis
    else do
      -- Initial Result
      initRes <- SC.newResult
      -- Process the data
      SC.Result (st, out) <- SC.process tree initRes
      if not $ null out
        then mapM_ putStrLn $ reverse out
        else do
          return ()
          -- text <- sDisplay st
          -- mapM_ putStrLn text
