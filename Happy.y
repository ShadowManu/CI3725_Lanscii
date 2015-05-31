{
	module Happy
	( parser
	, parsr
	) where

	import Alex (alexScanTokens)
	import Data.List
}

%name parsr
%tokentype {Token TkType}
%error {parseError}


%token 

  -- Basics
  '{' 				{ Token LCURLY _}
  '}' 				{ Token RCURLY _}
  '|' 				{ Token PIPE _}

  -- Constants
  true      		{ Token TRUE _}
  false     		{ Token FALSE _}
  '<>'        		{ Token CANVAS _}
  '<|>'        		{ Token CANVAS _}
  '</>'        		{ Token CANVAS _}
  '<_>'        		{ Token CANVAS _}
  '<->'        		{ Token CANVAS _}
  '<\>'        		{ Token CANVAS _}
  '#'        		{ Token CANVAS _}
  -- Reserved Words
  read 				{ Token READ _}
  write 			{ Token WRITE _}

  -- Type Symbols
  '%' 				{ Token PERCENT _}
  '!' 				{ Token EXCLAMATIONMARK _}
  '@' 				{ Token AT _}

  -- Common Operators
  '=' 				{ Token EQUALS _}
  ':' 				{ Token COLON _}
  ';' 				{ Token SEMICOLON _}
  '?' 				{ Token QUESTIONMARK _}
  '(' 				{ Token LPARENTHESIS _}
  ')' 				{ Token RPARENTHESIS _}
  '[' 				{ Token LBRACKET _}
  ']' 				{ Token RBRACKET _}
  '..' 				{ Token RANGE _}

  -- Boolean Operators
  '\/' 				{ Token LOG_OR _}
  '/\\' 			{ Token LOG_AND _}
  '^' 				{ Token LOG_NEG _}

  -- Relational Operators
  '<=' 				{ Token REL_LE _}
  '>=' 				{ Token REL_GE _}
  '/='				{ Token REL_NE _}
  '<'				{ Token REL_LT _}
  '>'				{ Token REL_GT _}

  -- Arithmetic Operators
  '+' 				{ Token PLUS _}
  '-' 				{ Token MINUS _}
  '*' 				{ Token ASTERISK _}
  '/' 				{ Token SLASH _}

  -- Canvas Operators
  '~' 				{ Token LINKING _}
  '&' 				{ Token AMPERSAND _}
  '$' 				{ Token DOLLAR _}
  '\'' 				{ Token APOSTROPHE _}

  id 				{ Token IDENTIFIER _}
  num 		  		{ Token NUMBER _}
---- Operator Precedence ----

-- bool precedence
%left '\/' 
%left '/\\'

-- relational
%nonassoc '<' '<=' '>' '>='
%nonassoc '/='

-- integer
%left '+' '-'
%left '*' '/' '%'

-- canvas
%left '&' '~' 

-- unary
%left '^'
%right '$'
%left '\''

%%

Begin
	:	Statement 			{$1}


Expression
	:	Expression '+' Expression 	{$1 $3}
	|	Expression '-' Expression 	{$1 $3}
	|	Expression '*' Expression   {$1 $3}
	|	Expression '/' Expression   {$1 $3}
	|	Expression '%' Expression   {$1 $3}

	|	Expression '<' Expression   {$1 $3}
	|	Expression '>' Expression   {$1 $3}
	|	Expression '<=' Expression  {$1 $3}
	|	Expression '>=' Expression  {$1 $3}
	|	Expression '/=' Expression  {$1 $3}

	| Expression '\/' Expression	{$1 $3}
	| Expression '/\\' Expression	{$1 $3}

	|	Expression '~' Expression   {$1 $3}
	|	Expression '&' Expression   {$1 $3}

	|	 '$' Expression             { $2 }
	|	 '-' Expression 			{ $2}

	|	Expression '^' 				{ $1}
	|	Expression '\'' 				{ $1}

	| '(' Expression ')' 	{$2}
	| true 				{}
	| false 			{}
	| num				{}					
	| id 				{}
	| '<>'				{}
	| '<|>'				{}
	| '<\>'				{}
	| '</>'				{}
	| '<_>'				{}
	| '<->'				{}
	| '#'				{}

Statement_List
	: Statement 						{$1}
	| Statement_List ';' Statement      {$1  $3}

Statement
	: '{' Declare_List '|' Statement_List '}' 		{$2 $4}
	| '{' Statement_List '}' 						{$2}

	| id '=' Expression 					{$1 $3 }

	| read id 								{}					
	| write id								{}

	-- Conditional statement
	| '(' Expression '?' Statement_List ')' 					{$2 $4}					
	| '(' Expression '?' Statement_List ':' Statement_List ')'	{$2 $4}

    -- Loop statement
	|'[' Expression '..' Expression '|' Statement_List ']'					{$2 $4 $6}
	|'[' id ':' Expression '..' Expression '|' Statement_List ']'	{$4 $6 $8}

Declare_List
	: Data_Type Declare_Id 					{$1 $2}
	| Declare_List Data_Type Declare_Id		{$1 $2 $3}

Declare_Id
	: id							{}
	| Declare_Id id 				{$1}

Data_Type
	: '%'					{}	
	| '@' 					{}
	| '!' 					{}
