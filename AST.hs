module AST
( Begin(..)
, Statement(..)
, DeclareList(..)
, DataType(..)
, Identifier(..)
, Expression(..)
, BinaryOp(..)
, UnaryOp(..)
) where

import Alex

data Begin = Statement
  deriving (Show, Eq)

data Statement
  = BlockStmt DeclareList [Statement]
  | Assignment Identifier Expression
  | Read Identifier
  | Write Identifier
  | If Expression Statement (Maybe Statement)
  | For (Maybe Identifier) Expression Statement
  deriving (Eq, Show)

data DeclareList = DeclareList [(DataType, Identifier)]
  deriving (Eq, Show)

data DataType
  = BoolType
  | IntType
  | CanvasType
  | TypeError [String]
  deriving (Show, Eq)

data Identifier = Identifier String
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
  | NotEqual
  | LessEq
  | GreatEq
  | ConcatH
  | ConcatV
  | Or
  | And
  | Range
  deriving (Show, Eq)

data UnaryOp
  = Rotate
  | Negate
  | Traspose
  | Logic_Negate
  deriving (Show, Eq)

-------------------------------
