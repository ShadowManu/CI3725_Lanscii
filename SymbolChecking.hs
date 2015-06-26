module SymbolChecking
( Process(..)
, Result(..)
, newResult
) where

import Prelude hiding (lookup)
import Data.Maybe
import Data.Either
import Data.List

import qualified SymbolTable as ST
import Alex
import AST
import Display

-- Type for keeping results of Symbol Table processing
newtype Result = Result (ST.SymbolTable, [String])

-- Result error getter
getErrors :: Result -> [String]
getErrors (Result (_, errs)) = errs

-- Append an error to a Result
appendError :: Result -> String -> Result
appendError (Result (st, out)) extra =
  Result (st, if extra `elem` out then out else extra:out)

-- Get the start expresssion of a range
startRange :: Range -> Expression
startRange (Range e1 _ _) = e1

-- Increase the start range of a range
nextRange :: Range -> Range
nextRange (Range e1@(IntExp val pos1) e2 apos) = (Range (IntExp (val+1) pos1) e2 apos)

-- Check if a range has no more elements to check inside
emptyRange :: Range -> Bool
emptyRange (Range (IntExp val1 _) (IntExp val2 _) _) = if val1 > val2 then True else False
emptyRange _ = True

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
  process (BlockStmt decList stmtList) res@(Result (st, out)) = do
    newSt <- ST.newScope st
    Result (nextSt, nextOut) <- process decList (Result (newSt, out)) >>= process stmtList
    lastSt <- ST.closeScope nextSt
    return $ Result (lastSt, nextOut)

  process (Assignment (Identifier iden _) expr apos) res@(Result (st, out)) = do
    sym <- ST.lookupComplete st iden
    if isJust sym
    then do
      rhsTypeRes <- getExpType expr res
      if myIsRight rhsTypeRes && (ST.getType (fromJust sym) == myFromRight rhsTypeRes)
      then do
        newRes@(Result (newSt, newOut)) <- process expr res
        newExp <- evalExp expr newRes
        nextSt <- ST.update newSt iden ((\(Right rhsType) -> ST.Symbol iden rhsType newExp True) rhsTypeRes)
        return $ Result (nextSt, newOut)
      else
        let extra = "Type of assignment at line " ++ show (posLine apos) ++ " have incompatible types."
        in return $ appendError res extra
    else
      let extra = "Variable " ++ show iden ++ " at the left side of assignment in line " ++ show (posLine apos) ++ " has not been declared."
      in process expr $ appendError res extra

  process (Read (Identifier iden _) pos) res@(Result (st, out)) = do
    sym <- ST.lookupComplete st iden
    if isJust sym
    then do
      let expType = ST.getType $ fromJust sym
      newSt <- case expType of
        IntType -> getLine >>= return . (read :: String -> Integer) >>= (\x -> ST.update st iden (ST.Symbol iden expType (IntExp x pos) True))
        BoolType -> getLine >>= return . (read :: String -> Integer) >>= (\x -> ST.update st iden (ST.Symbol iden expType (IntExp x pos) True))
        CanvasType -> ST.statError (getErrors res) $ "Read with Canvas type."
      return res
    else
      let extra = "Variable " ++ show iden ++ " used in a Read statement in line " ++ show (posLine pos) ++ "is not declared in the scope"
      in return $ appendError res extra

  process (Write expr pos) res = do
    expType <- getExpType expr res
    case expType of
      Right CanvasType -> do
        newRes <- process expr res
        (CanvasExp val _) <- evalExp expr newRes
        mapM_ putStrLn val
        return newRes
      _ -> let extra = "Expression of Write in line " ++ show (posLine pos) ++ "is not of type Canvas."
        in process expr $ appendError res extra

  process (If expr thenList elseList pos) res = do
    expType <- getExpType expr res
    case expType of
      Right BoolType -> do
        newRes <- process expr res
        (BoolExp val _) <- evalExp expr newRes
        case val of
          True -> process thenList newRes
          False -> process elseList newRes
      _ -> let extra = "Expression in If statement at line " ++ show (posLine pos) ++ " is not of type Bool."
        in process expr res

  process for@(ForIn expr stList pos) res = do
    newRes <- process expr res
    expType <- getExpType expr newRes
    case expType of
      Right BoolType -> do
        (BoolExp bool _) <- evalExp expr res
        if bool then process stList newRes >>= process for else return newRes
      _ -> let extra = "Expression in For statement at line " ++ show (posLine pos) ++ " is not of type Bool."
        in return $ appendError newRes extra

  process (ForDet Nothing range stList pos) res =
    case range of
      (Range (IntExp _ _) (IntExp _ _) _) -> do
        process range res >>= process stList >>= processWhenValid
          where
            processWhenValid r = if emptyRange range
              then return r
              else process (ForDet Nothing (nextRange range) stList pos) r
      (Range e1 e2 rpos) -> do
        exp1 <- evalExp e1 res
        exp2 <- evalExp e1 res
        process range res >>= process (ForDet Nothing (Range exp1 exp2 rpos) stList pos)

  process (ForDet var@(Just ider@(Identifier iden _)) range stList apos) res@(Result (st, out)) =
    case range of
      (Range (IntExp _ _) (IntExp _ _) _) -> do
        newRes@(Result (newSt, newOut)) <- process range res
        sym <- ST.lookup newSt iden
        case sym of
          Just _ -> let extra = "Variable " ++ show iden ++ " used as enumerated identifier in For statement is already declared in the local scope"
            in return $ appendError newRes extra
          Nothing ->
            if emptyRange range
            then
              return newRes
            else do
              valExp <- evalExp (startRange range) newRes
              nextSt <- ST.insert newSt iden (ST.Symbol iden IntType valExp True)
              (Result (lastSt, lastOut)) <- process stList (Result (nextSt, newOut))
              ultiSt <- ST.delete nextSt iden
              process (ForDet var (nextRange range) stList apos) (Result (ultiSt, lastOut))
      (Range e1 e2 rpos) -> do
        exp1 <- evalExp e1 res
        exp2 <- evalExp e1 res
        process range res >>= process (ForDet var (Range exp1 exp2 rpos) stList apos)

-- Process variable declarations
instance Process (DataType, Identifier) where
  process (dt, Identifier iden pos) (Result (st, out)) = do
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
      newSt <- ST.insert st iden (ST.Symbol iden dt (defVal BoolType pos)  False)
      return $ Result (newSt, out)

-- Omitting processing DataTypes (not required)
-- Ommiting processing Identifiers (they are embedded in other instances)

-- Process range expressions
instance Process Range where
  process (Range e1 e2 pos) res = do
    process e1 res >>= process e2 >>= checkt1 >>= checkt2
    where
      checkt1 r = do
        t1Res <- getExpType e1 r
        case t1Res of
          Right IntType -> return r
          _ -> ST.statError (getErrors res) $ "Start expression in Range at line " ++ show (posLine pos) ++ " is not of type Int."
      checkt2 r = do
        t2Res <- getExpType e2 r
        case t2Res of
          Right IntType -> return r
          _ -> ST.statError (getErrors res) $ "End expression in Range at line " ++ show (posLine pos) ++ " is not of type Int."

-- Helper because isRight is not defined in older Haskell Libraries
myIsRight :: Either a b -> Bool
myIsRight (Left _) = False
myIsRight (Right _) = True

-- Helper for Right Values
myFromRight :: Either b a -> a
myFromRight (Right x) = x

-- Process Expressions
instance Process Expression where
  process expr res@(Result (st, out)) = do
    t <- getExpType expr res
    -- When the type of the expression is correct
    if myIsRight t
    -- There is nothing to report
    then return res
    -- If it isnt, report the (first found) problem on the expression
    else case t of
      -- Reporting Undeclared variable in expression
      Left (Undeclared var, expr) ->
        let extra = "Variable " ++ var ++ " not declared in expression: " ++ show expr
        in return $ Result (st, extra:out)
      -- Reporting Incompatible binary expressions
      Left (Incompatible, expr@(BinaryExp op e1 e2 _)) ->
        let extra = "Incompatible use of operator " ++ show op ++ " between expressions " ++ show e1 ++ " and " ++ show e2
        in return $ Result (st, extra:out)
      -- Reporting Incompatible binary expressions
      Left (Incompatible, expr@(UnaryExp op e _)) ->
        let extra = "Incompatible use of operator " ++ show op ++ " with the expression " ++ show e
        in return $ Result (st, extra:out)

-- Type Checking of Expressions

data ExprError = Undeclared String | Incompatible
type ExpTypeResult = IO (Either (ExprError, Expression) DataType)

-- Main Type checker function
getExpType :: Expression -> Result -> ExpTypeResult
getExpType (BinaryExp op e1 e2 _) res = compatibleBinExp op e1 e2 res
getExpType (UnaryExp op e _) res = compatibleUnExp op e res
getExpType (IntExp _ _) _ = return $ Right IntType
getExpType (BoolExp _ _) _ = return $ Right BoolType
getExpType (CanvasExp _ _) _ = return $ Right CanvasType
getExpType var@(VarExp (Identifier iden _) _) res@(Result (st, out)) = do
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
compatibleBinExp op e1@(VarExp (Identifier iden api) _) e2 res@(Result (st, out))= do
  sym <- ST.lookupComplete st iden
  if isJust sym
  then
    compatibleBinExp op (defVal (ST.getType $ fromJust sym) api) e2 res
  else
    return $ Left (Undeclared iden, e1)

compatibleBinExp op e1 e2@(VarExp (Identifier iden api) _) res@(Result (st, out))= do
  sym <- ST.lookupComplete st iden
  if isJust sym
  then
    compatibleBinExp op e1 (defVal (ST.getType $ fromJust sym) api) res
  else
    return $ Left (Undeclared iden, e2)

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
    -- Wrong type propagation (undeclared over incompatible -being the latest, the default-)
    (val@(Left (Undeclared _, _)), _, _) -> return val
    (_, _, val@(Left (Undeclared _, _))) -> return val
    (val@(Left (Incompatible, _)), _, _) -> return val
    (_, _, val@(Left (Incompatible, _))) -> return val
    -- Wrong type operation
    _ -> return $ Left (Incompatible, BinaryExp op e1 e2 (position e1))

-- Type Checker for unary expressions
compatibleUnExp :: UnaryOp -> Expression -> Result -> ExpTypeResult
compatibleUnExp op e@(VarExp (Identifier iden api) _) res@(Result (st, out)) = do
  sym <- ST.lookupComplete st iden
  -- If the symbol exists
  if isJust sym
  then
    -- Evaluate expression with type found substituted
    compatibleUnExp op (defVal (ST.getType $ fromJust sym) api) res
  else
    -- Report undeclared value
    return $ Left (Undeclared iden, e)
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
    -- Wrong type propagation
    (_, val@(Left (Undeclared _, _))) -> return val
    (_, val@(Left (Incompatible, _))) -> return val
    -- Wrong type expression
    _ -> return $ Left (Incompatible, UnaryExp op e (position e))

-- Default Values for Expressions types
defVal :: DataType -> AlexPosn -> Expression
defVal BoolType = BoolExp False
defVal IntType = IntExp 0
defVal CanvasType = CanvasExp []

-- Expression Evaluation with dynamic checks
evalExp :: Expression -> Result -> IO Expression
evalExp (BinaryExp op e1 e2 pos) res = do
  ex1 <- evalExp e1 res
  ex2 <- evalExp e2 res
  case (ex1, op, ex2) of
    -- Logical
    (BoolExp False _, Or, BoolExp False _) -> return $ BoolExp False pos
    (BoolExp _ _, Or, BoolExp _ _) -> return $ BoolExp True pos
    (BoolExp True _, And, BoolExp True _) -> return $ BoolExp True pos
    (BoolExp _ _, And, BoolExp _ _) -> return $ BoolExp False pos
    -- Arithmetic
    (left, Plus, right) -> opOver (+) left right res
    (left, Minus, right) -> opOver (subtract) left right res
    (left, Times, right) -> opOver (*) left right res
    (IntExp val1 _, Div, IntExp val2 _) ->
      if val2 /= 0 then return $ IntExp (val1 `div` val2) pos
        else ST.statError (getErrors res) $ "Division by zero in line " ++ (show $ posLine pos)
    (IntExp val1 _, Mod, IntExp val2 _) ->
      if val2 /= 0 then do
        return $ IntExp (val1 `mod` val2) pos
        else ST.statError (getErrors res) $ "Mod Division by zero in line " ++ (show $ posLine pos) ++ "."
    -- Relational
    (IntExp val1 _, LessT, IntExp val2 _) -> return $ BoolExp (val1<val2) pos
    (IntExp val1 _, GreatT, IntExp val2 _) -> return $ BoolExp (val1>val2) pos
    (IntExp val1 _, Equal, IntExp val2 _) -> return $ BoolExp (val1==val2) pos
    (IntExp val1 _, NotEqual, IntExp val2 _) -> return $ BoolExp (val1/=val2) pos
    (IntExp val1 _, LessEq, IntExp val2 _) -> return $ BoolExp (val1<=val2) pos
    (IntExp val1 _, GreatEq, IntExp val2 _) -> return $ BoolExp (val1>=val2) pos
    -- Canvas
    (CanvasExp val1 _, ConcatH, CanvasExp val2 _) ->
      if length val1 == length val2 then return $ CanvasExp (zipWith (++) val1 val2) pos
        else ST.statError (getErrors res) $ "Canvas horizontal concatenation at line " ++ (show $ posLine pos) ++ " has operators with different heights."
    (CanvasExp [] _, ConcatV, CanvasExp [] _) -> return $ CanvasExp [] pos
    (CanvasExp val1 _, ConcatV, CanvasExp val2 _) ->
      if (not $ null val1) && (not $ null val2) && (length $ head val1) == (length $ head val2)
        then return $ CanvasExp (val1 ++ val2) pos
        else ST.statError (getErrors res) $ "Canvas vertical concatenation at line " ++ (show $ posLine pos) ++ " has operators with different widths."

evalExp (UnaryExp op e pos) res = do
  ex1 <- evalExp e res
  case (op, e) of
    -- Arithmetic
    (Negative, IntExp val _) -> return $ IntExp (-val) pos
    -- Boolean
    (Negate, BoolExp val _) -> return $ BoolExp (not val) pos
    -- Canvas
    (Rotate, CanvasExp val _) -> return $ CanvasExp (map (map rotate) val) pos
    (Transpose, CanvasExp val _) -> return $ CanvasExp (transpose val) pos

evalExp (VarExp (Identifier iden _) pos) res@(Result (st, out)) = do
  (Just sym) <- ST.lookupComplete st iden
  if ST.isInit sym
  then
    return $ ST.getValue sym
  else
    ST.statError (getErrors res) $ "Variable " ++ show iden ++ " in line " ++ show (posLine pos) ++ " used in expression without being initialized."

evalExp x res = return x -- Int, Boolean and Canvas Expressions are already evaluated

-- Helper for overflow
opOver :: (Integer -> Integer -> Integer) -> Expression -> Expression -> Result -> IO Expression
opOver op e1@(IntExp val1 pos1) e2@(IntExp val2 pos2) res =
  case op val1 val2 of final
                              | mini <= final && final <= maxi -> return $ IntExp final pos1
                              | otherwise -> ST.statError (getErrors res) $ "Arithmetic overflow at line " ++ show (posLine pos1) ++ "."
  where
    mini = toInteger (minBound :: Int)
    maxi = toInteger (maxBound :: Int)

-- Helper for rotate operation
rotate :: Char -> Char
rotate '/' = '\\'
rotate '\\' = '/'
rotate '-' = '|'
rotate '_' = '|'
rotate '|' = '-'
rotate ' ' = ' '
