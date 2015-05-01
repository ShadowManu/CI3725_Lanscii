{
module Main (main) where
import Data.List
}

%wrapper "posn"

$digit = 0-9 -- digits

@identifier = [a-zA-Z_][a-zA-Z_0-9]*
@integer = [0-9]+

-- Helpers for comments
@lcom = \{\-
@rcom = \-\}
@notcom = [^\-\}]|$white

tokens :-

  -- Basics
  $white+ ;
  \{ { Token LCURLY }
  \} { Token RCURLY }
  \| { Token PIPE }
  -- Alex does not accept lookaheads/lookbehinds
  -- The following expresion can be interpreted as follow:
  -- We start with a left comment symbol
  -- We end with a right comment symbol
  -- And in the middle we only accept groups of -  and }
  -- with any other symbol that separates them (including any whitespaces)
  @lcom\-*(\-*@notcom+\}*)*\}*@rcom ; -- Comment

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
  = { Token EQUALS }
  \: { Token COLON }
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
  -- == ; { Token REL_EQ } -- Does not exist in spec
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

  -- Unexpected symbols
  . { Token BAD_CHAR }
  @lcom { Token BAD_COMMENT }
  @rcom { Token BAD_COMMENT }

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
  IDENTIFIER |
  BAD_CHAR | BAD_COMMENT
  deriving (Show,Eq)

-- Define the general token structure:
-- the type of the token, its value, and position information
data Token = Token TkType AlexPosn String
  deriving Eq

-- How a token is printed (showed)
instance Show Token where
  show (Token tktype (AlexPn abs ln cn) value) =
    "token " ++ show tktype ++ " value (" ++ value ++ ") at line: " ++ show ln ++ ", column: " ++ show cn

-- Helper for Canvas Constants
canToken :: String -> AlexPosn -> String -> Token
canToken s = (\apos _ -> Token CANVAS apos s)

-- Determines which tokens are considered bad
isBadToken :: Token -> Bool
isBadToken (Token BAD_CHAR _ _) = True
isBadToken (Token BAD_COMMENT _ _) = True
isBadToken x = False

-- Given a list of tokens, gives only bad tokens if there is at least 1 bad
tokensFilter :: [Token] -> [Token]
tokensFilter toks = if find (isBadToken) toks == Nothing
  then toks else filter (isBadToken) toks

main = do
  s <- getContents
  (mapM_ print . tokensFilter . alexScanTokens) s
}
