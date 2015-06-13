module SymbolChecking
where

import Prelude hiding (lookup)
import Data.Maybe

import qualified SymbolTable as ST
import AST
import Display

-- Type for keeping results of Symbol Table processing
newtype Result = Result (ST.SymbolTable, [String])

-- Typeclass for common processing of symbols
class Process a where
  process :: a -> Result -> IO Result

-- Common instance for maybe types
instance (Process a) => Process (Maybe a) where
  process (Just x) res = process x res
  process Nothing res = return res

-- Common instance for list types
instance (Process a) => Process [a] where
  process [] res = return res
  process (x:xs) res = do
    newRes <- process x res
    process xs newRes

-- Process a program
instance Process Begin where
  process (Begin stmt) res = process stmt res

-- Process statements
instance Process Statement where
  -- TODO a Block statement is new scope: should use a new table
  process (BlockStmt decList stmtList) res = do
    newRes <- process decList res
    process stmtList newRes
  process (Assignment (Identifier iden) expr) res@(Result (st, out)) = do
    -- Check if the identifier is in scope (possibly not local)
    sym <- ST.lookupComplete st iden
    if isNothing sym
    -- If it is, just check the expression
    then process expr res
    -- iF its not, add an error and check the expression
    else
      let extra = "Variable " ++ show iden ++ " used in the left side of an \
      \assignment is not declared in the scope."
      in process expr (Result (st, extra:out))
  -- #! TODO COMPLETE OTHER PATTERNS
  process (Read (Identifier iden)) res@(Result (st, out)) = do
    -- Check if the identifier is in scope (possibly not local)
    sym <- ST.lookupComplete st iden
    if isNothing sym
    -- If it is, keep processing
    then return res
    -- iF its not, add an error and check the expression
    else
      let extra = "Variable " ++ show iden ++ " used in a Read statement is not \
      \declared in the scope"
      in return $ Result (st, extra:out)
  process (Write (Identifier iden)) res@(Result (st, out)) = do
    -- Check if the identifier is in scope (possibly not local)
    sym <- ST.lookupComplete st iden
    if isNothing sym
    -- If it is, keep processing
    then return res
    -- iF its not, add an error and check the expression
    else
      let extra = "Variable " ++ show iden ++ " used in a Write statement is not \
      \declared in the scope"
      in return $ Result (st, extra:out)
  process (If expr thenList elseList) res =
    -- Just check the 3 components
    process expr res >>= process thenList >>= process elseList

  process (ForIn expr stList) res =
    -- Just check the 2 components
    process expr res >>= process stList
  process (ForDet Nothing range stList) res = do
    -- Check the range
    newRes <- process range res
    -- Check the statement list
    process stList newRes
  process (ForDet (Just ider@(Identifier iden)) range stList) res@(Result (st, out)) = do
    -- Check the range first (it must not use the enumerator variable)
    newRes <- process range res
    -- Check the identifier in the local scope TODO verify scope rules
    let (Result (newSt, newOut)) = newRes
    sym <- ST.lookup newSt iden
    let extra = "Variable " ++ show iden ++ " used as enumerated \
    \identifier in For statement is already declared in the local scope"
    nextRes <- if isJust sym
    -- If found, add an error
    then return $ Result (newSt, extra:newOut)
    -- If its not, add it to the scope
    else process (IntType, ider) newRes
    -- Keep checking with the other pattern
    process (ForDet Nothing range stList) nextRes

-- Process variable declarations
instance Process (DataType, Identifier) where
  process (dt, Identifier iden) (Result (st, out)) = do
    -- Check if variable is declared locally already (should not)
    sym <- ST.lookup st iden
    if isJust sym
    -- If it is, add an error
    then
      let extra = "Variable " ++ show iden ++ " is already declared in the scope \
      \ of a declaration list."
      in return $ Result (st, extra:out)
    -- If its not, add it to the table (hiding the upper one)
    else do
      newSt <- ST.insert st iden (ST.Symbol iden dt)
      return $ Result (newSt, out)

-- Omitting processing DataTypes (not required)
-- Ommiting processing Identifiers (they are embedded in other instances)

-- Process range expressions
instance Process Range where
  process (Range e1 e2) res =
    -- Just check both Expressions
    process e1 res >>= process e2

-- Process Expressions
instance Process Expression where
  process (BinaryExp op e1 e2) res = do
    newRes <- process e1 res
    process e2 newRes
  -- #! TODO COMPLETE OTHER PATTERNS
