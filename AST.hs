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
  | BoolExp Bool
  | CanvasExp [String]
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
  | Traspose
  | Negate
  deriving (Show, Eq)

-------------------------------
