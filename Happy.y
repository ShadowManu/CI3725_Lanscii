{
	module Happy
	( parser
	, parsr
	) where

	import AST ( Begin(..)
			   , Statement(..)
			   , Statement_List(..)
			   , Declare_Id(..)
			   , Declare_List(..)
			   , Data_Type(..)
			   , Expression(..)
			   )

	import Alex (alexScanTokens)
}

%name parsr
%tokentype {Token}
%error {parseError}

tokens :-

  -- Basics
  \{ 				{ Token LCURLY _}
  \} 				{ Token RCURLY _}
  \| 				{ Token PIPE _}

  -- Constants
  true      		{ Token TRUE _}
  false     		{ Token FALSE _}
  \#        		{ Token CANVAS _}

  -- Reserved Words
  read 				{ Token READ _}
  write 			{ Token WRITE _}

  -- Type Symbols
  \% 				{ Token PERCENT _}
  \! 				{ Token EXCLAMATIONMARK _}
  \@ 				{ Token AT _}

  -- Common Operators
  =  				{ Token EQUALS _}
  \: 				{ Token COLON _}
  \; 				{ Token SEMICOLON _}
  \? 				{ Token QUESTIONMARK _}
  \( 				{ Token LPARENTHESIS _}
  \) 				{ Token RPARENTHESIS _}
  \[ 				{ Token LBRACKET _}
  \] 				{ Token RBRACKET _}
  \.\. 				{ Token RANGE _}

  -- Boolean Operators
  \\\/ 				{ Token LOG_OR _}
  \/\\ 				{ Token LOG_AND _}
  \^ 				{ Token LOG_NEG _}

  -- Relational Operators
  \<= 				{ Token REL_LE _}
  \>= 				{ Token REL_GE _}
  \/= 				{ Token REL_NE _}
  \<  				{ Token REL_LT _}
  \>  				{ Token REL_GT _}

  -- Arithmetic Operators
  \+ 				{ Token PLUS _}
  \- 				{ Token MINUS _}
  \* 				{ Token ASTERISK _}
  \/ 				{ Token SLASH _}
  \% 				{ Token PERCENT _}

  -- Canvas Operators
  \~ 				{ Token LINKING _}
  \& 				{ Token AMPERSAND _}
  \$ 				{ Token DOLLAR _}
  \' 				{ Token APOSTROPHE _}

  id 				{ Token IDENTIFIER _}
  num 		  		{ Token NUMBER _}
---- Operator Precedence ----

-- boolean
%left \\\/
%left \/\\

-- relational
%nonassoc '<' '<=' '>' '>='
%nonassoc '/='

-- integer
%left '+' '-'
%left '*' '/' '%'

-- canvas
%left '&' '~' 

-- unary
%left ^
%right '$'
%left '''

%%

Begin
	:	Statement 			{$1}


Expression
	:	Expression '+' Expression 	{Binary (Plus  (tp $2)) $1 $3}
	|	Expression '-' Expression 	{Binary (Minus (tp $2)) $1 $3}
	|	Expression '*' Expression   {Binary (Times (tp $2)) $1 $3}
	|	Expression '/' Expression   {Binary (Div   (tp $2)) $1 $3}
	|	Expression '%' Expression   {Binary (Mod   (tp $2)) $1 $3}

	|	Expression '<' Expression   {Binary (LessT   (tp $2)) $1 $3}
	|	Expression '>' Expression   {Binary (GreatT  (tp $2)) $1 $3}
	|	Expression '<=' Expression  {Binary (LessEq  (tp $2)) $1 $3}
	|	Expression '>=' Expression  {Binary (GreatEq (tp $2)) $1 $3}
	|	Expression '/=' Expression  {Binary (Unequal (tp $2)) $1 $3}

	|	Expression '~' Expression   {Binary (ConcatH (tp $2)) $1 $3}
	|	Expression '&' Expression   {Binary (ConcatV (tp $2)) $1 $3}

	|	Expression '\/' Expression  {Binary (Or  (tp $2)) $1 $3}
	|	Expression '/\' Expression  {Binary (And (tp $2)) $1 $3}

	|	 '$' Expression             {Unary (Rotate (tp $1)) $2 }
	|	 '-' Expression 			{Unary (Negate (tp $2)) $2}

	|	Expression '^' 				{Unary (Logic_Negate (tp $2)) $1}
	|	Expression ''' 				{Unary (Traspose (tp $2)) $1}

	| '(' Expression ')' 	{$2}
	| NUMBER				{IntConst (extract $1)}
	| Bool 					{BoolConst $1}
	| IDENTIFIER 			{Var $1} 


Bool
	: true 					{true}
	| false 				{false}

Statement_List
	: Statement 						{$1}
	| Statement_List ';' Statement      {$1 : $3}

Statement
	: '{' Declare_List '|' Statement_List '}' 		{Block $2 $4}
	| '{' Statement_List '}' 						{Block [] $2}

	| IDENTIFIER '=' Expression 					{Equals $1 $3 ((\(Id_p) -> p) $1)}

	| READ IDENTIFIER 								{Read $2 (tp $1)}					
	| WRITE IDENTIFIER								{Write $2 (tp $1)}

	-- Conditional statement
	| '(' Expression '?' Statement_List ')' 					{If $2 $4 Nothing}					
	| '(' Expression '?' Statement_List ':' Statement_List ')'	{If $2 $4 (Just $7)}

    -- Loop statement
	|'[' Expression '..' Expression '|' Statement_List ']'					{For $2 $4 $6}
	|'[' IDENTIFIER ':' Expression '..' Expression '|' Statement_List ']'	

Declare_List
	: Data_Type Declare_Id 					{map (Declare_List $1) $2}
	| Declare_List Data_Type Declare_Id		

Declare_Id
	: IDENTIFIER 							{Id (extract $1) (tp $1)}
	| Declare_Id IDENTIFIER 

Data_Type
	: PERCENT		{IntType}
	| AT 			{BoolType}
	| CANVAS 		{CanvasType}

{
	extract :: Token -> Int
	extract (Token NUMBER n _) = n

	parseError :: [Token] -> a
	parseError l = case l of
		[] -> error $ "Unexpected EOF"
		_  -> error $ "Unexpected " ++ show (head l)

	parser :: String -> String -> IO ()
	parser text name = do
		putStrLn $ "Parser (" ++ name ++ "):\n"
		let toks = alexScanTokens text
		if any isTokenError toks
			then mapM_ printError $ filter isTokenError toks
			else putStrLn . show . parsr $ toks
		putStrLn ""
}







