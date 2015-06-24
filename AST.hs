module AST
( Begin(..)
, Statement(..)
, DeclareList(..)
, DataType(..)
, Identifier(..)
, Range(..)
, Expression(..)
, BinaryOp(..)
, UnaryOp(..)
, Position(..)
) where

import Alex
import Display

data Begin = Begin Statement
  deriving (Show, Eq)

data Statement
  = BlockStmt (Maybe DeclareList) StatementList
  | Assignment Identifier Expression AlexPosn
  | Read Identifier AlexPosn
  | Write Identifier AlexPosn
  | If Expression StatementList (Maybe StatementList) AlexPosn
  | ForIn Expression StatementList AlexPosn
  | ForDet (Maybe Identifier) Range StatementList AlexPosn
  deriving (Show, Eq)

type DeclareList = [(DataType, Identifier)]

type StatementList = [Statement]

data DataType
  = BoolType
  | IntType
  | CanvasType
  deriving (Show, Eq)

data Identifier = Identifier String AlexPosn
  deriving (Show, Eq)

data Range = Range Expression Expression AlexPosn
  deriving (Show, Eq)

data Expression
  = BinaryExp BinaryOp Expression Expression AlexPosn
  | UnaryExp UnaryOp Expression AlexPosn
  | IntExp Int AlexPosn
  | VarExp Identifier AlexPosn
  | BoolExp Bool AlexPosn
  | CanvasExp [String] AlexPosn
  deriving (Show, Eq)

data BinaryOp
  = Plus
  | Minus
  | Div
  | Mod
  | Times
  | LessT
  | GreatT
  | Equal
  | NotEqual
  | LessEq
  | GreatEq
  | ConcatH
  | ConcatV
  | Or
  | And
  deriving (Show, Eq)

data UnaryOp
  = Rotate
  | Negative
  | Transpose
  | Negate
  deriving (Show, Eq)

---- Parser Position instances
class Position a where
  position :: a -> AlexPosn

instance Position Statement where
  position (Assignment _ _ p) = p
  position (Read _ p) = p
  position (Write _ p) = p
  position (If _ _ _ p) = p
  position (ForIn _ _ p) = p
  position (ForDet _ _ _ p) = p

instance Position Identifier where
  position (Identifier _ p) = p

instance Position Range where
  position (Range _ _ p) = p

instance Position Expression where
  position (BinaryExp _ _ _ p) = p
  position (UnaryExp _ _ p) = p
  position (IntExp _ p) = p
  position (VarExp _ p) = p
  position (BoolExp _ p) = p
  position (CanvasExp _ p) = p

instance Position Token where
  position = tokPos

---- Parser Display instances and Helpers

-- Prefix Indentation
prefix :: String
prefix = " |  "

-- Indenter helper
indented :: (PDisplay a) => a -> [String]
indented = map (prefix ++) . pDisplay

-- Common instance for Maybe types
instance (PDisplay a) => PDisplay (Maybe a) where
  pDisplay (Just x) = pDisplay x
  pDisplay Nothing = []

-- Common instance for List types
instance (PDisplay a) => PDisplay ([a]) where
  pDisplay = concatMap pDisplay

-- Instance for Begin
instance PDisplay Begin where
  pDisplay (Begin st) = pDisplay st

-- Instance for Statement
instance PDisplay Statement where
  pDisplay (BlockStmt _ sl) = pDisplay sl -- Ommited Declaration list
  pDisplay (Assignment iden expr _) = concat [["ASSIGN:"], indented iden, indented expr]
  pDisplay (Read iden _) = concat [["READ:"], indented iden]
  pDisplay (Write iden _) = concat [["WRITE:"], indented iden]
  pDisplay (If expr sl1 sl2 _) = concat [["CONDITIONAL STATEMENT:"], indented expr, indented sl1, indented sl2]
  pDisplay (ForIn expr sl _) = concat [["INDETERMINED ITERATION STATEMENT:"], indented expr, indented sl]
  pDisplay (ForDet iden range sl _) = concat [["DETERMINED ITERATION STATEMENT:"], indented iden, indented range, indented sl]

-- Ommiting DeclareList
-- Ommiting StatementList
-- Ommiting DataType

-- Instance for Identifier
instance PDisplay Identifier where
  pDisplay (Identifier str _) = ["IDENTIFIER: " ++ str]

-- Instance for Range
instance PDisplay Range where
  pDisplay (Range s e _) = concat [["RANGE:"], start, end]
    where
      start = map (prefix ++) . concat $ [["START:"], indented s]
      end = map (prefix ++) .concat $ [["END:"], indented e]

instance PDisplay Expression where
  pDisplay (BinaryExp op e1 e2 _) = concat [["BINARY EXPRESSION"], indented op, indented e1, indented e2]
  pDisplay (UnaryExp op e _) = concat [["UNARY EXPRESSION"], indented op, indented e]
  pDisplay (IntExp num _) = ["NUMBER: " ++ show num]
  pDisplay (VarExp iden _) = pDisplay iden
  pDisplay (BoolExp bool _) = ["BOOLEAN: " ++ show bool]
  pDisplay (CanvasExp strs _) = "CANVAS: " : strs

instance PDisplay BinaryOp where
  pDisplay Plus = ["OPERATOR: +"]
  pDisplay Minus = ["OPERATOR: -"]
  pDisplay Div = ["OPERATOR: /"]
  pDisplay Mod = ["OPERATOR: %"]
  pDisplay Times = ["OPERATOR: *"]
  pDisplay LessT = ["OPERATOR: <"]
  pDisplay GreatT = ["OPERATOR: >"]
  pDisplay Equal = ["OPERATOR: ="]
  pDisplay NotEqual = ["OPERATOR: /="]
  pDisplay LessEq = ["OPERATOR: <="]
  pDisplay GreatEq = ["OPERATOR: >="]
  pDisplay ConcatH = ["OPERATOR: ~"]
  pDisplay ConcatV = ["OPERATOR: &"]
  pDisplay Or = ["OPERATOR: \\/"]
  pDisplay And = ["OPERATOR: /\\"]

instance PDisplay UnaryOp where
  pDisplay Rotate = ["OPERATOR: $"]
  pDisplay Negative = ["OPERATOR: -"]
  pDisplay Transpose = ["OPERATOR: \'"]
  pDisplay Negate = ["OPERATOR: ^"]
