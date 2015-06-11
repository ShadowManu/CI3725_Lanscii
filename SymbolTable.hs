module SymbolTable
where

{- OPTIONS_GHC -XFlexibleInstances -XTypeSynonymInstances -}

import qualified Data.HashTable.IO as H
import qualified Data.HashTable.ST.Basic as B
import qualified GHC.Prim as P

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
data Symbol = Symbol String
  deriving (Eq, Show)

-- Concrete type for the Hash Table for a local Symbol Table
type Table = H.BasicHashTable String Symbol

-- Type for a complete Symbol Table
newtype SymbolTable = SymbolTable (Tree Table, TreeTrail Table)

-- Type Synonym for SymbolTable printing
type Results = [(String, Symbol)]

-----------------------------------
---- Symbol Table minimal functions
-----------------------------------

-- New Symbol Table
new :: IO SymbolTable
new = do
  newTable <- H.new
  return (SymbolTable (Tree newTable [], []))

-- Insert an element on a Symbol Table
insert :: String -> Symbol -> SymbolTable -> IO SymbolTable
-- #! TESTING TODO complete
insert str sym st@(SymbolTable (Tree tab childs, trail)) = do
  H.insert tab str sym
  return st

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
    let strKv (k,v) = k ++ " -- " ++ show v -- TODO Replace
    let printKv = map strKv
    return . printKv $ kvList
