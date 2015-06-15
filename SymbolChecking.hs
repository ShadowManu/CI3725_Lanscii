module SymbolChecking
( Process(..)
, Result(..)
, newResult
) where

import Prelude hiding (lookup)
import Data.Maybe
import Data.Either

import qualified SymbolTable as ST
import AST
import Display

-- Type for keeping results of Symbol Table processing
newtype Result = Result (ST.SymbolTable, [String])

-- Result Constructor
newResult :: IO (Result)
newResult = do
  initTable <- ST.new
  return $ Result (initTable, [])

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
  process (BlockStmt decList stmtList) res@(Result (st, out)) = do
    newSt <- ST.openScope st
    Result (nextSt, nextOut) <- process decList (Result (newSt, out)) >>= process stmtList
    lastSt <- ST.closeScope nextSt
    return $ Result (lastSt, nextOut)
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
    -- Check both expressions
    process e1 res >>= process e2

-- Process Expressions
instance Process Expression where
  process expr res@(Result (st, out)) = do
    t <- getExpType expr res
    -- When the type of the expression is correct
    if isRight t
    -- There is nothing to report
    then return res
    -- If it isnt, report the (first found) problem on the expression
    else case t of
      -- Reporting Undeclared variable in expression
      Left (Undeclared var, expr) ->
        let extra = "Variable " ++ var ++ " not declared in expression: " ++ show expr
        in return $ Result (st, extra:out)
      -- Reporting Incompatible binary expressions
      Left (Incompatible, expr@(BinaryExp op e1 e2)) ->
        let extra = "Incompatible use of operator " ++ show op ++ " between expressions " ++ show e1 ++ " and " ++ show e2
        in return $ Result (st, extra:out)
      -- Reporting Incompatible binary expressions
      Left (Incompatible, expr@(UnaryExp op e)) ->
        let extra = "Incompatible use of operator " ++ show op ++ " with the expression " ++ show e
        in return $ Result (st, extra:out)

-- Type Checking of Expressions

data ExprError = Undeclared String | Incompatible
type ExpTypeResult = IO (Either (ExprError, Expression) DataType)

-- Main Type checker function
getExpType :: Expression -> Result -> ExpTypeResult
getExpType (BinaryExp op e1 e2) res = compatibleBinExp op e1 e2 res
getExpType (UnaryExp op e) res = compatibleUnExp op e res
getExpType (NumExp _) _ = return $ Right IntType
getExpType (BoolExp _) _ = return $ Right BoolType
getExpType (CanvasExp _) _ = return $ Right CanvasType
getExpType var@(VarExp (Identifier iden)) res@(Result (st, out)) = do
  sym <- ST.lookup st iden
  return
    -- If the symbol is defined
    (if isJust sym
    -- Use its type as the return value
    then Right . ST.getType . fromJust $ sym
    -- If not, then no type is correct
    else Left (Undeclared iden, var))

-- Type Checker helper for binary epressions
compatibleBinExp :: BinaryOp -> Expression -> Expression -> Result -> ExpTypeResult
compatibleBinExp op e1 e2 res = do
  t1 <- getExpType e1 res
  t2 <- getExpType e2 res
  case (t1, op, t2) of
  -- Logical
    (Right BoolType, Or, Right BoolType) -> return $ Right BoolType
    (Right BoolType, And, Right BoolType) -> return $ Right BoolType
    -- Arithmetic
    (Right IntType, Plus, Right IntType) -> return $ Right IntType
    (Right IntType, Minus, Right IntType) -> return $ Right IntType
    (Right IntType, Times, Right IntType) -> return $ Right IntType
    (Right IntType, Div, Right IntType) -> return $ Right IntType
    (Right IntType, Mod, Right IntType) -> return $ Right IntType
    -- Relational
    (Right IntType, LessT, Right IntType) -> return $ Right BoolType
    (Right IntType, GreatT, Right IntType) -> return $ Right BoolType
    (Right IntType, Equal, Right IntType) -> return $ Right BoolType
    (Right IntType, NotEqual, Right IntType) -> return $ Right BoolType
    (Right IntType, LessEq, Right IntType) -> return $ Right BoolType
    (Right IntType, GreatEq, Right IntType) -> return $ Right BoolType
    -- Canvas
    (Right CanvasType, ConcatH, Right CanvasType) -> return $ Right CanvasType
    (Right CanvasType, ConcatV, Right CanvasType) -> return $ Right CanvasType
    -- Wrong type expressions
    (val@(Left (Undeclared _, _)), _, _) -> return val
    (_, _, val@(Left (Undeclared _, _))) -> return val
    (val@(Left (Incompatible, _)), _, _) -> return val
    (_, _, val@(Left (Incompatible, _))) -> return val
    _ -> return $ Left (Incompatible, BinaryExp op e1 e2)

-- Type Checker for unary expressions
compatibleUnExp :: UnaryOp -> Expression -> Result -> ExpTypeResult
compatibleUnExp op e res = do
  t <- getExpType e res
  case (op, t) of
  -- Arithmetic
    (Negative, Right IntType) -> return $ Right IntType
    -- Canvas
    (Rotate, Right CanvasType) -> return $ Right CanvasType
    (Transpose, Right CanvasType) -> return $ Right CanvasType
    -- Boolean
    (Negate, Right BoolType) -> return $ Right BoolType
    -- Wrong Type Expressions
    (_, val@(Left (Undeclared _, _))) -> return val
    (_, val@(Left (Incompatible, _))) -> return val
    _ -> return $ Left (Incompatible, UnaryExp op e)
