-- File: Happy.txt
-- Description: lanscii language basic parser
-- Authors:
--     Manuel Pacheco - 10-10524
--     Nicolas Mañan - 06-39883
{
module Happy (happyParser) where

import Alex
import AST
import Display
}

%name happyParser
%tokentype { Token }
%error { parseError }

%token

  -- Basics
  '{' { Token LCURLY ap $$ }
  '}' { Token RCURLY ap $$ }
  '|' { Token PIPE ap $$ }

  -- Constants
  true  { Token TRUE ap $$ }
  false { Token FALSE ap $$ }
  int  { Token NUMBER ap $$ }
  '#'    { Token CANVAS ap $$ }
  '<\>'  { Token CANVAS ap $$ }
  '<|>'  { Token CANVAS ap $$ }
  '</>'  { Token CANVAS ap $$ }
  '<->'  { Token CANVAS ap $$ }
  '<_>'  { Token CANVAS ap $$ }
  '< >'  { Token CANVAS ap $$ }

  -- Reserved Words
  read  { Token READ ap $$ }
  write { Token WRITE ap $$ }

  -- Type Symbols
  '\%' { Token PERCENT ap $$ }
  '!' { Token EXCLAMATIONMARK ap $$ }
  '@' { Token AT ap $$ }

  -- Common Operators
  '='   { Token EQUALS ap $$ }
  ':'   { Token COLON ap $$ }
  ';'   { Token SEMICOLON ap $$ }
  '?'   { Token QUESTIONMARK ap $$ }
  '('   { Token LPARENTHESIS ap $$ }
  ')'   { Token RPARENTHESIS ap $$ }
  '['   { Token LBRACKET ap $$ }
  ']'   { Token RBRACKET ap $$ }
  '..'  { Token RANGE ap $$ }

  -- Boolean Operators
  '\\/'  { Token LOG_OR ap $$ }
  '/\\'  { Token LOG_AND ap $$ }
  '^'   { Token LOG_NEG ap $$ }

  -- Relational Operators
  '<='  { Token REL_LE ap $$ }
  '>='  { Token REL_GE ap $$ }
  '/='  { Token REL_NE ap $$ }
  '<'   { Token REL_LT ap $$ }
  '>'   { Token REL_GT ap $$ }

  -- Arithmetic Operators
  '+' { Token PLUS ap $$ }
  '-' { Token MINUS ap $$ }
  '*' { Token ASTERISK ap $$ }
  '/' { Token SLASH ap $$ }

  -- Canvas Operators
  '~' { Token LINKING ap $$ }
  '&' { Token AMPERSAND ap $$ }
  '$' { Token DOLLAR ap $$ }
  '\'' { Token APOSTROPHE ap $$ }

  -- Normal Symbols
  identifier { Token IDENTIFIER ap $$ }

---- Operator Precedence ----

-- Bool precedence
%left '\\/'
%left '/\\'

-- Relational
%nonassoc '<' '<=' '>' '>='
%nonassoc '/='

-- Integer
%left '+' '-'
%left '*' '/' '%'

-- Canvas
%left '&' '~'

-- Unary
%left '^'
%right '$'
%left '\''

%%

BEGIN : STATEMENT { Begin $1 }

STATEMENT : '{' DECLARE_LIST '|' STATEMENT_LIST '}'  { BlockStmt (Just $2) $4 }
  | '{' STATEMENT_LIST '}'  { BlockStmt Nothing $2 }
  | IDENTIFIER '=' EXPRESSION { Assignment $1 $3 }
  | read IDENTIFIER { Read $2 }
  | write IDENTIFIER { Write $2 }

  | '(' EXPRESSION '?' STATEMENT_LIST ')' { If $2 $4 Nothing }
  | '(' EXPRESSION '?' STATEMENT_LIST ':' STATEMENT_LIST ')' { If $2 $4 (Just $6) }

  | '[' EXPRESSION '|' STATEMENT_LIST '}' { ForIn $2 $4 }
  | '[' EXPRESSION '..' EXPRESSION '|' STATEMENT_LIST ']' { ForDet Nothing (Range $2 $4) $6 }
  | '[' IDENTIFIER ':' EXPRESSION '..' EXPRESSION '|' STATEMENT_LIST ']' { ForDet (Just $2) (Range $4 $6) $8 }

STATEMENT_LIST : STATEMENT { [$1] }
  | STATEMENT_LIST ';' STATEMENT  { $1 ++ [$3] }

DECLARE_LIST : DATA_TYPE DECLARE_ID  { map (\x ->($1,x)) $2 }
  | DECLARE_LIST DATA_TYPE DECLARE_ID { (map (\x ->($2,x)) $3) ++ $1 }

DATA_TYPE : '\%'  { IntType }
  | '@' { CanvasType }
  | '!' { BoolType }

DECLARE_ID : IDENTIFIER { [$1] }
  | DECLARE_ID IDENTIFIER { $1 ++ [$2] }

IDENTIFIER : identifier { Identifier $1 }

EXPRESSION : int  { NumExp (read $1 :: Int) }
  | IDENTIFIER { VarExp $1 }
  | true  { BoolExp True }
  | false { BoolExp False }
  | '#' { CanvasExp "" }
  | '<\>' { CanvasExp "\\" }
  | '<|>' { CanvasExp "|" }
  | '</>' { CanvasExp "/" }
  | '<->' { CanvasExp "-" }
  | '<_>' { CanvasExp "_" }
  | '< >' { CanvasExp " " }
  | '(' EXPRESSION ')' { $2 }

  -- Arithmetic Operators
  | EXPRESSION '+' EXPRESSION { BinaryExp Plus $1 $3 }
  | EXPRESSION '-' EXPRESSION { BinaryExp Minus $1 $3 }
  | EXPRESSION '*' EXPRESSION { BinaryExp Times $1 $3 }
  | EXPRESSION '/' EXPRESSION { BinaryExp Div $1 $3 }
  | EXPRESSION '\%' EXPRESSION { BinaryExp Mod $1 $3 }

  -- Unary Arithmetic Operator
  | '-' EXPRESSION  { UnaryExp Negative $2 }

  -- Canvas Operators
  | EXPRESSION '~' EXPRESSION { BinaryExp ConcatH $1 $3 }
  | EXPRESSION '&' EXPRESSION { BinaryExp ConcatV $1 $3 }

  -- Unary Canvas Operator
  | '$' EXPRESSION { UnaryExp Rotate $2}
  | EXPRESSION '\'' { UnaryExp Transpose $1 }

  -- Relational Operators
  | EXPRESSION '<=' EXPRESSION { BinaryExp LessEq $1 $3 }
  | EXPRESSION '>=' EXPRESSION { BinaryExp GreatEq $1 $3 }
  | EXPRESSION '/=' EXPRESSION { BinaryExp NotEqual $1 $3 }
  | EXPRESSION '<' EXPRESSION { BinaryExp LessT $1 $3 }
  | EXPRESSION '>' EXPRESSION { BinaryExp GreatT $1 $3 }
  | EXPRESSION '=' EXPRESSION { BinaryExp Equal $1 $3 }

  -- Boolean Operators
  | EXPRESSION '\\/' EXPRESSION { BinaryExp Or $1 $3 }
  | EXPRESSION '/\\' EXPRESSION { BinaryExp And $1 $3 }

  -- Unary Boolean Operator
  | EXPRESSION '^' { UnaryExp Negate $1 }

{

parseError :: [Token] -> a
parseError x = error $ "Parse error: " ++ (display $ head x)

}
