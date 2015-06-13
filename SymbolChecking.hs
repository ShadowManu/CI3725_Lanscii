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
      let extra = "Variable " ++ show iden ++ " is not declared in the scope."
      in process expr (Result (st, extra:out))
  -- #! TODO COMPLETE OTHER PATTERNS

-- Process variable declarations
instance Process (DataType, Identifier) where
  process (dt, Identifier iden) (Result (st, out)) = do
    -- Check if variable is declared locally already (should not)
    sym <- ST.lookup st iden
    if isJust sym
    -- If it is, add an error
    then
      let extra = "Variable " ++ show iden ++ " is already declared in the scope."
      in return $ Result (st, extra:out)
    -- If its not, add it to the table (hiding the upper one)
    else do
      newSt <- ST.insert st iden (ST.Symbol iden dt)
      return $ Result (newSt, out)

-- Process Expressions
instance Process Expression where
  process (BinaryExp op e1 e2) res = do
    newRes <- process e1 res
    process e2 newRes
  -- #! TODO COMPLETE OTHER PATTERNS
