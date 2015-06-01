-- File: Alex.x
-- Description: lanscii language lexer
-- Authors:
--     Manuel Pacheco - 10-10524
--     Nicolas Mañan - 06-39883

{
module Alex
( Token(..)
, TkType(..)
, noParseToken
, tokenize
) where

import Display

}

%wrapper "posn"

$digit = 0-9 -- digits

@identifier = [a-zA-Z_][a-zA-Z_0-9]*
@integer = [0-9]+

-- Helpers for comments
@lcom = \{\-
@rcom = \-\}
@ex = [^\-\}]|$white

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
{

-- Define the general token structure:
-- the type of the token, its value, and position information
data Token = Token TkType AlexPosn String
  deriving (Show, Eq)

-- Defines the different types of available tokens
-- There should be one for each different symbol type
data TkType =
  LCURLY | RCURLY | PIPE |
  TRUE | FALSE | NUMBER | CANVAS |
  READ | WRITE |
  PERCENT | EXCLAMATIONMARK | AT |
  EQUALS | COLON | SEMICOLON | QUESTIONMARK | LPARENTHESIS | RPARENTHESIS | LBRACKET | RBRACKET | RANGE |
  LOG_OR | LOG_AND | LOG_NEG |
  REL_LE | REL_GE | REL_NE | REL_LT | REL_GT |
  PLUS | MINUS | ASTERISK | SLASH | -- PERCENT already included
  AMPERSAND | LINKING |  DOLLAR | APOSTROPHE |
  IDENTIFIER |
  BAD_CHAR | BAD_LCOM | BAD_RCOM
  deriving (Show,Eq)

-- How a token is displayed (printed)
instance Display Token where
  display (Token BAD_CHAR (AlexPn abs ln cn) value) =
    "Error: Unexpected character: \"" ++ value ++ "\" at line: " ++ show ln ++ ", column: " ++ show cn
  display (Token BAD_LCOM (AlexPn abs ln cn) value) =
    "Error: Comment section opened but not closed at line: " ++ show ln ++ ", column: " ++ show cn
  display (Token BAD_RCOM (AlexPn abs ln cn) value) =
    "Error: Comment section closed without opening at line: " ++ show ln ++ ", column: " ++ show cn
    -- Not in spec but decided to add
  display (Token tktype (AlexPn abs ln cn) value) =
    "token " ++ show tktype ++ " value (" ++ value ++ ") at line: " ++ show ln ++ ", column: " ++ show cn

-- Helper for Canvas Constants
canToken :: String -> AlexPosn -> String -> Token
canToken s apos _ = Token CANVAS apos s

-- Determines which tokens are considered bad
isBadToken :: Token -> Bool
isBadToken (Token BAD_CHAR _ _) = True
isBadToken x = False

-- Determines which tokes are considered bad comment
isBadComment :: Token -> Bool
isBadComment (Token BAD_LCOM _ _) = True
isBadComment (Token BAD_RCOM _ _) = True
isBadComment x = False

-- Helper for token users
noParseToken :: Token -> Bool
noParseToken x = isBadToken x || isBadComment x

-- Given a list of tokens, gives only good tokens,
-- bad char tokens or bad comment tokens
-- with priority over the last ones
tokensFilter :: [Token] -> [Token]
tokensFilter = choice . foldr send ([],[],[])
  where
    send x (a,b,c)
      | isBadComment x = (a,b,x:c)
      | isBadToken x = (a,x:b,c)
      | otherwise = (x:a,b,c)
    choice (a,b,c)
      | not $ null c = c
      | not $ null b = b
      | otherwise = a

tokenize :: String -> [Token]
tokenize = tokensFilter . alexScanTokens

}
