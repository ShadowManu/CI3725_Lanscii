{
module Main (main) where
}

%wrapper "posn"

$digit = 0-9 -- digits

@identifier = [a-zA-Z_][a-zA-Z_0-9]*
@comment = \{\-.*\-\}
@integer = [0-9]+

tokens :-

  -- Basics
  $white+ ;
  \{ { Token LCURLY }
  \} { Token RCURLY }
  \| { Token PIPE }
  @comment ;

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

  -- Normal Operators
  = { Token EQUALS }
  \; { Token SEMICOLON }
  \? { Token QUESTIONMARK }
  \( { Token LPARENTHESIS }
  \) { Token RPARENTHESIS }

  -- Boolean Operators
  \\\/ { Token LOG_OR }
  \/\\ { Token LOG_AND }
  \^ { Token LOG_NEG }

  -- Relational Operators
  \<= { Token REL_LE }
  \>= { Token REL_GE }
  -- == ; { Token REL_EQ } -- TODO Does not exist in spec
  \/= { Token REL_NE }
  -- \< { Token REL_LT }
  -- \> { Token REL_GT }

  -- Arithmetic Operators
  \+ { Token PLUS }
  \- { Token MINUS }
  \* { Token ASTERISK }
  \/ { Token SLASH }
  \% { Token PERCENT }

  -- Canvas Operators
  \: { Token COLON }
  \| { Token PIPE }
  \$ { Token DOLLAR }
  \' { Token APOSTROPHE }

  -- Normal Symbols
    @identifier { Token IDENTIFIER }
  -- TODO True False
  -- TODO FIx Number
  -- TODO CANVAS CONSTANTS
{

-- Defines the different types of available tokens
-- There should be one for each different symbol type
data TkType =
  LCURLY | RCURLY | PIPE |
  TRUE | FALSE | NUMBER | CANVAS |
  READ | WRITE |
  PERCENT | EXCLAMATIONMARK | AT |
  EQUALS | SEMICOLON | QUESTIONMARK | LPARENTHESIS | RPARENTHESIS |
  LOG_OR | LOG_AND | LOG_NEG |
  REL_LE | REL_GE | REL_NE | REL_LT | REL_GT |
  PLUS | MINUS | ASTERISK | SLASH | -- PERCENT already included
  COLON |  DOLLAR | APOSTROPHE | -- PIPE already included
  IDENTIFIER
  deriving Show

-- Define the general token structure:
-- the type of the token, its value, and position information
data Token = Token TkType AlexPosn String

-- How a token is printed (showed)
instance Show Token where
  show (Token tktype (AlexPn abs ln cn) value) =
    "token " ++ show tktype ++ " value (" ++ value ++ ") at line: " ++ show ln ++ ", column: " ++ show cn

-- Helper for Canvas Constants
canToken :: String -> AlexPosn -> String -> Token
canToken s = (\apos _ -> Token CANVAS apos s)

main = do
  s <- getContents
  mapM_ print (alexScanTokens s)
}
