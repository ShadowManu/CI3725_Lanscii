module SymbolTable
( Symbol(..)
, SymbolTable
, new
, insert
, lookup
, lookupComplete
, openScope
, closeScope
) where

{- OPTIONS_GHC -XFlexibleInstances -XTypeSynonymInstances -}

import Prelude hiding (lookup)
import Data.Maybe

import qualified Data.HashTable.IO as H
import qualified Data.HashTable.ST.Basic as B
import qualified GHC.Prim as P

import AST
import Display

-----------------------------------------
---- Auxiliar data types for Zipper model
-----------------------------------------

-- Generic Tree type
data Tree a = Tree a [Tree a]

-- Trail type for tree reconstruction
type TreeTrail a = [Tree a]

---------------------------------------
---- Data types to model a Symbol table
---------------------------------------

-- Type for symbol information
-- TODO to be fully implemented
data Symbol = Symbol {
  getIden :: String,
  getType :: DataType,
  getValue :: Expression,
  isInit :: Bool}
  deriving (Eq, Show)

-- Concrete type for the Hash Table for a local Symbol Table
type Table = H.BasicHashTable String Symbol

-- Type for a complete Symbol Table
newtype SymbolTable = SymbolTable (Tree Table, TreeTrail Table)

-- Type Synonym for SymbolTable printing
type Results = [(String, Symbol)]

------------------------------------------------------------------
---- Symbol Table functions operating directly with the hashtables
------------------------------------------------------------------

-- New Symbol Table
new :: IO SymbolTable
new = do
  newTable <- H.new
  return (SymbolTable (Tree newTable [], []))

-- Insert an element on a Symbol Table
insert :: SymbolTable -> String -> Symbol -> IO SymbolTable
insert st@(SymbolTable (Tree tab childs, trail)) str sym = do
  H.insert tab str sym
  return st

-- Get a symbol in the local Symbol Table (if it exists) using a key
lookup :: SymbolTable -> String -> IO (Maybe Symbol)
lookup st@(SymbolTable (Tree tab childs, trail)) = H.lookup tab

-- Get a symbol in the Symbol Tables in scope (if it exists) using a key
lookupComplete :: SymbolTable -> String -> IO (Maybe Symbol)
lookupComplete st@(SymbolTable (local, [])) str = do
  -- Local lookup
  sym <- lookup st str
  return
    (if isJust sym
      -- If found, send it
      then sym
      -- If not, we looked everywhere already
      else Nothing)
lookupComplete st@(SymbolTable (local, Tree parent siblings : rest)) str = do
  -- Local lookup
  sym <- lookup st str
  -- If found
  if isJust sym then return sym
  -- If not, we can keep looking
  else lookupComplete (SymbolTable (Tree parent (local:siblings), rest)) str

openScope :: SymbolTable -> IO SymbolTable
openScope (SymbolTable (local, rest)) = do
  newTable <- H.new
  return $ SymbolTable (Tree newTable [], local:rest)

closeScope :: SymbolTable -> IO SymbolTable
closeScope (SymbolTable (local, Tree parent siblings : rest)) =
  return $ SymbolTable (Tree parent (local:siblings), rest)

-----------------------------------
---- Display instances por printing
-----------------------------------

-- Prefix Indentation
prefix :: String
prefix = " |  "

-- Indentation helper
indent :: [String] -> [String]
indent = map (prefix ++)

-- Common instance for List types
instance (SDisplay a) => SDisplay ([a]) where
  sDisplay xs = do
    let ioList = map sDisplay xs
    results <- sequence ioList
    return . concat $ results

instance SDisplay SymbolTable where
  sDisplay (SymbolTable (tree, trail)) = sDisplay tree

instance (SDisplay a) => SDisplay (Tree a) where
  sDisplay (Tree tab xs) = do
    tabText <- sDisplay tab
    xsText <- sDisplay xs
    return $ tabText ++ indent xsText

-- Instance to display hashtables
-- The specific type was inferred by GHC 7.8.3
instance SDisplay (B.HashTable P.RealWorld String Symbol) where
  sDisplay hash = do
    kvList <- H.toList hash
    let strKv (_, Symbol name t val ini) = "Symbol " ++ show name ++ " with type " ++ show t ++ "."
    let printKv = map strKv
    return $ "SYMBOLS: " : printKv kvList
