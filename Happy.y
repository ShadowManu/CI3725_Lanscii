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
  $white+ ;
  \{ { Token LCURLY }
  \} { Token RCURLY }
  \| { Token PIPE }
  -- The following expresion can be interpreted as follow:
  -- We start with a left comment symbol
  -- after which we can have either
  --   1. Any not-comment character
  --   2. A } character
  --   3. A group of at least 1 - followed by any char in rule 1.
  -- finishing with a group of at least 1 -  followed by }
  -- This expression has been derived from an automaton
  \{\-(@ex|\}|\-+@ex)*\-+\} ; -- Comment

  -- Constants
  true      { Token TRUE }
  false     { Token FALSE }
  @integer  { Token NUMBER }
  \#        { Token CANVAS }
  \<\\\>    { canToken "\\" }
  \<\|\>    { canToken "|" }
  \<\/\>    { canToken "/" }
  \<\-\>    { canToken "-" }
  \<\_\>    { canToken "_" }
  \<\ \>    { canToken " " }

  -- Reserved Words
  read { Token READ }
  write { Token WRITE }

  -- Type Symbols
  \% { Token PERCENT }
  \! { Token EXCLAMATIONMARK }
  \@ { Token AT }

  -- Common Operators
  =  { Token EQUALS }
  \: { Token COLON }
  \; { Token SEMICOLON }
  \? { Token QUESTIONMARK }
  \( { Token LPARENTHESIS }
  \) { Token RPARENTHESIS }
  \[ { Token LBRACKET }
  \] { Token RBRACKET }
  \.\. { Token RANGE }

  -- Boolean Operators
  \\\/ { Token LOG_OR }
  \/\\ { Token LOG_AND }
  \^ { Token LOG_NEG }

  -- Relational Operators
  \<= { Token REL_LE }
  \>= { Token REL_GE }
  \/= { Token REL_NE }
  \<  { Token REL_LT }
  \>  { Token REL_GT }

  -- Arithmetic Operators
  \+ { Token PLUS }
  \- { Token MINUS }
  \* { Token ASTERISK }
  \/ { Token SLASH }
  \% { Token PERCENT }

  -- Canvas Operators
  \~ { Token LINKING }
  \& { Token AMPERSAND }
  \$ { Token DOLLAR }
  \' { Token APOSTROPHE }

  -- Normal Symbols
    @identifier { Token IDENTIFIER }

  -- Unexpected symbols
  . { Token BAD_CHAR }
  @lcom { Token BAD_LCOM }
  @rcom { Token BAD_RCOM } -- Spec does not specify this as bad comment
                           -- But any other alternatives are bad

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
	: '{' Declare_List '|' Statement_List '}'
	| '{' Statement_List '}'

	| IDENTIFIER '=' Expression

	| READ IDENTIFIER
	| WRITE IDENTIFIER

	-- if like, statement
	| '(' Expression '?' Statement_List ')'
	| '(' Expression '?' Statement_List ':' Statement_List ')'

    -- for like, statement
	|'[' Expression '..' Expression '|' Statement_List ']'
	|'[' IDENTIFIER ':' Expression '..' Expression '|' Statement_List ']'

Declare_List
	: Data_Type Declare_Id 					{map (Declare_List $1) $2}
	| Declare_List Data_Type Declare_Id		{}

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







