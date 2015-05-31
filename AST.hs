module AST
( Begin(..)
, Statement(..)
, Declare_List(..)
, DataType(..)
, Id(..)
, Expression(..)
, BinOp(..)
, UnOp(..)
,tabs
) where

import Data.List (intercalate)

import Alex

data Begin = Statement deriving (Eq)

data Statement
	= Equals Id Expression Pos
	| Block [Declare_List] [Statement_List] Pos
	| Read Id Por
	| Write Id Pos
	| If	Expression Statement_List (Alt Statement_List) Pos
	| For 	(Alt Id) Expression (Alt Expression) Statement_List
	deriving (Eq, Show)

data Declare_List = Declare_List DataType Id deriving (Eq, Show)


data DataType 
      = BoolType
			|	IntType
			|	CanvasType
			|	TypeError [String]
			deriving (Eq)

data Id = Id String Pos deriving(Eq)

data Expression
	= Binary	BinaryOp	Expression Expression
	| Unary		UnnaryOp	Expression
	| IntConst	Int 
	| Var		Id
	deriving (Eq, Show)

data BinaryOp
	= Plus		
	| Minus		
	| Div 		
	| Mod 		
	| Times 	
	| LessT		
	| GreatT	
	| Unequal 	
	| LessEq		
  | GreatEq	
	| ConcatH	
	| ConcatV	
	| Or		
	| And		
	deriving (Eq)

data UnaryOp
	= Rotate 		
	| Negate		
	| Traspose  
	| Logic_Negate
	deriving (Eq)

-------------------------------

