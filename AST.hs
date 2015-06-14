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
) where

import Display

data Begin = Begin Statement
  deriving (Show, Eq)

data Statement
  = BlockStmt (Maybe DeclareList) StatementList
  | Assignment Identifier Expression
  | Read Identifier
  | Write Identifier
  | If Expression StatementList (Maybe StatementList)
  | ForIn Expression StatementList
  | ForDet (Maybe Identifier) Range StatementList
  deriving (Show, Eq)

type DeclareList = [(DataType, Identifier)]

type StatementList = [Statement]

data DataType
  = BoolType
  | IntType
  | CanvasType
  | TypeError [String]
  deriving (Show, Eq)

data Identifier = Identifier String
  deriving (Show, Eq)

data Range = Range Expression Expression
  deriving (Show, Eq)

data Expression
  = BinaryExp BinaryOp Expression Expression
  | UnaryExp UnaryOp Expression
  | NumExp Int
  | VarExp Identifier
  | BoolExp Bool
  | CanvasExp String
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
  pDisplay (Assignment iden expr) = concat [["ASSIGN:"], indented iden, indented expr]
  pDisplay (Read iden) = concat [["READ:"], indented iden]
  pDisplay (Write iden) = concat [["WRITE:"], indented iden]
  pDisplay (If expr sl1 sl2) = concat [["CONDITIONAL STATEMENT:"], indented expr, indented sl1, indented sl2]
  pDisplay (ForIn expr sl) = concat [["INDETERMINED ITERATION STATEMENT:"], indented expr, indented sl]
  pDisplay (ForDet iden range sl) = concat [["DETERMINED ITERATION STATEMENT:"], indented iden, indented range, indented sl]

-- Ommiting DeclareList
-- Ommiting StatementList
-- Ommiting DataType

-- Instance for Identifier
instance PDisplay Identifier where
  pDisplay (Identifier str) = ["IDENTIFIER: " ++ str]

-- Instance for Range
instance PDisplay Range where
  pDisplay (Range s e) = concat [["RANGE:"], start, end]
    where
      start = map (prefix ++) . concat $ [["START:"], indented s]
      end = map (prefix ++) .concat $ [["END:"], indented e]

instance PDisplay Expression where
  pDisplay (BinaryExp op e1 e2) = concat [["BINARY EXPRESSION"], indented op, indented e1, indented e2]
  pDisplay (UnaryExp op e) = concat [["UNARY EXPRESSION"], indented op, indented e]
  pDisplay (NumExp num) = ["NUMBER: " ++ show num]
  pDisplay (VarExp iden) = pDisplay iden
  pDisplay (BoolExp bool) = ["BOOLEAN: " ++ show bool]
  pDisplay (CanvasExp str) = ["CANVAS: " ++ str]

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
