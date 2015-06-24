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
  '{' { Token LCURLY ap val }
  '}' { Token RCURLY ap val }
  '|' { Token PIPE ap val }

  -- Constants
  true  { Token TRUE ap val }
  false { Token FALSE ap val }
  int  { Token NUMBER ap val }
  '#'    { Token CANVAS ap val }
  '<\>'  { Token CANVAS ap val }
  '<|>'  { Token CANVAS ap val }
  '</>'  { Token CANVAS ap val }
  '<->'  { Token CANVAS ap val }
  '<_>'  { Token CANVAS ap val }
  '< >'  { Token CANVAS ap val }

  -- Reserved Words
  read  { Token READ ap val }
  write { Token WRITE ap val }

  -- Type Symbols
  '\%' { Token PERCENT ap val }
  '!' { Token EXCLAMATIONMARK ap val }
  '@' { Token AT ap val }

  -- Common Operators
  '='   { Token EQUALS ap val }
  ':'   { Token COLON ap val }
  ';'   { Token SEMICOLON ap val }
  '?'   { Token QUESTIONMARK ap val }
  '('   { Token LPARENTHESIS ap val }
  ')'   { Token RPARENTHESIS ap val }
  '['   { Token LBRACKET ap val }
  ']'   { Token RBRACKET ap val }
  '..'  { Token RANGE ap val }

  -- Boolean Operators
  '\\/'  { Token LOG_OR ap val }
  '/\\'  { Token LOG_AND ap val }
  '^'   { Token LOG_NEG ap val }

  -- Relational Operators
  '<='  { Token REL_LE ap val }
  '>='  { Token REL_GE ap val }
  '/='  { Token REL_NE ap val }
  '<'   { Token REL_LT ap val }
  '>'   { Token REL_GT ap val }

  -- Arithmetic Operators
  '+' { Token PLUS ap val }
  '-' { Token MINUS ap val }
  '*' { Token ASTERISK ap val }
  '/' { Token SLASH ap val }

  -- Canvas Operators
  '~' { Token LINKING ap val }
  '&' { Token AMPERSAND ap val }
  '$' { Token DOLLAR ap val }
  '\'' { Token APOSTROPHE ap val }

  -- Normal Symbols
  identifier { Token IDENTIFIER ap val }

---- Operator Precedence ----

-- Bool precedence
%left '\\/'
%left '/\\'

-- Relational
%nonassoc '<' '<=' '>' '>='
%nonassoc '/=' '='

-- Integer
%left '+' '-'
%left '*' '/' '\%'

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
  | IDENTIFIER '=' EXPRESSION { Assignment $1 $3 (position $1) }
  | read IDENTIFIER { Read $2 (position $2) }
  | write IDENTIFIER { Write $2 (position $2) }

  | '(' EXPRESSION '?' STATEMENT_LIST ')' { If $2 $4 Nothing (position $2) }
  | '(' EXPRESSION '?' STATEMENT_LIST ':' STATEMENT_LIST ')' { If $2 $4 (Just $6) (position $2) }

  | '[' EXPRESSION '|' STATEMENT_LIST '}' { ForIn $2 $4 (position $2) }
  | '[' EXPRESSION '..' EXPRESSION '|' STATEMENT_LIST ']' { ForDet Nothing (Range $2 $4 (position $2)) $6 (position $2) }
  | '[' IDENTIFIER ':' EXPRESSION '..' EXPRESSION '|' STATEMENT_LIST ']' { ForDet (Just $2) (Range $4 $6 (position $4)) $8 (position $2) }

STATEMENT_LIST : STATEMENT { [$1] }
  | STATEMENT_LIST ';' STATEMENT  { $1 ++ [$3] }

DECLARE_LIST : DATA_TYPE DECLARE_ID  { map (\x ->($1,x)) $2 }
  | DECLARE_LIST DATA_TYPE DECLARE_ID { (map (\x ->($2,x)) $3) ++ $1 }

DATA_TYPE : '\%'  { IntType }
  | '@' { CanvasType }
  | '!' { BoolType }

DECLARE_ID : IDENTIFIER { [$1] }
  | DECLARE_ID IDENTIFIER { $1 ++ [$2] }

IDENTIFIER : identifier { Identifier (tokVal $1) (position $1) }

EXPRESSION : int { IntExp (read (tokVal $1) :: Int) (position $1) }
  | IDENTIFIER { VarExp $1 (position $1) }
  | true  { BoolExp True (position $1) }
  | false { BoolExp False (position $1) }
  | '#' { CanvasExp [""] (position $1) }
  | '<\>' { CanvasExp ["\\"] (position $1) }
  | '<|>' { CanvasExp ["|"] (position $1) }
  | '</>' { CanvasExp ["/"] (position $1) }
  | '<->' { CanvasExp ["-"] (position $1) }
  | '<_>' { CanvasExp ["_"] (position $1) }
  | '< >' { CanvasExp [" "] (position $1) }
  | '(' EXPRESSION ')' { $2 }

  -- Arithmetic Operators
  | EXPRESSION '+' EXPRESSION { BinaryExp Plus $1 $3 (position $1) }
  | EXPRESSION '-' EXPRESSION { BinaryExp Minus $1 $3 (position $1) }
  | EXPRESSION '*' EXPRESSION { BinaryExp Times $1 $3 (position $1) }
  | EXPRESSION '/' EXPRESSION { BinaryExp Div $1 $3 (position $1) }
  | EXPRESSION '\%' EXPRESSION { BinaryExp Mod $1 $3 (position $1) }

  -- Unary Arithmetic Operator
  | '-' EXPRESSION  { UnaryExp Negative $2 (position $1) }

  -- Canvas Operators
  | EXPRESSION '~' EXPRESSION { BinaryExp ConcatH $1 $3 (position $1) }
  | EXPRESSION '&' EXPRESSION { BinaryExp ConcatV $1 $3 (position $1) }

  -- Unary Canvas Operator
  | '$' EXPRESSION { UnaryExp Rotate $2 (position $1) }
  | EXPRESSION '\'' { UnaryExp Transpose $1 (position $1) }

  -- Relational Operators
  | EXPRESSION '<=' EXPRESSION { BinaryExp LessEq $1 $3 (position $1) }
  | EXPRESSION '>=' EXPRESSION { BinaryExp GreatEq $1 $3 (position $1) }
  | EXPRESSION '/=' EXPRESSION { BinaryExp NotEqual $1 $3 (position $1) }
  | EXPRESSION '<' EXPRESSION { BinaryExp LessT $1 $3 (position $1) }
  | EXPRESSION '>' EXPRESSION { BinaryExp GreatT $1 $3 (position $1) }
  | EXPRESSION '=' EXPRESSION { BinaryExp Equal $1 $3 (position $1) }

  -- Boolean Operators
  | EXPRESSION '\\/' EXPRESSION { BinaryExp Or $1 $3 (position $1) }
  | EXPRESSION '/\\' EXPRESSION { BinaryExp And $1 $3 (position $1) }

  -- Unary Boolean Operator
  | EXPRESSION '^' { UnaryExp Negate $1 (position $1) }

{

parseError :: [Token] -> a
parseError x = error $ "Parse error: " ++ (display $ head x)

}
