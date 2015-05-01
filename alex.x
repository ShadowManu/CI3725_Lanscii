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
  \{ ; -- TODO
  \} ; -- TODO
  \| ; -- TODO
  @comment { Token COMMENT } -- TODO REMOVE

  -- Constants
  @integer ; -- TODO
  \# ; -- TODO
  \<\\\> ; -- TODO
  \<\|\> ; -- TODO
  \<\/\> ; -- TODO
  \<\-\> ; -- TODO
  \<\_\> ; -- TODO
  \< \> ; -- TODO

  -- Reserved Words
  read ; -- TODO
  write ; -- TODO

  -- Type Symbols
  \% ; -- TODO
  \! ; -- TODO
  \@ ; -- TODO

  -- Normal Operators
  = ; -- TODO
  \; ; -- TODO
  \? ; -- TODO
  \( ; -- TODO
  \) ; -- TODO

  -- Boolean Operators
  \\\/ ; -- TODO OR
  \/\\ ; -- TODO AND
  \^ ; -- TODO

  -- Relational Operators
  \<= ; -- TODO
  \>= ; -- TODO
  == ; -- TODO
  \/= ; -- TODO
  \< ; -- TODO
  \> ; -- TODO

  -- Arithmetic Operators
  \+ ; -- TODO
  \- ; -- TODO
  \* ; -- TODO
  \/ ; -- TODO
  \% ; -- TODO

  -- Canvas Operators
  \: ; -- TODO
  \| ; -- TODO
  \$ ; -- TODO
  \' ; -- TODO

  -- Normal Symbols
  @identifier { Token IDENTIFIER }

{

-- Defines the different types of available tokens
-- There should be one for each different symbol type
data TkType =
  IDENTIFIER |
  COMMENT
  deriving Show

-- Define the general token structure:
-- the type of the token, its value, and position information
data Token = Token TkType AlexPosn String

-- How a token is printed (showed)
instance Show Token where
  show (Token tktype (AlexPn abs ln cn) value) =
    "token " ++ show tktype ++ " value (" ++ value ++ ") at line: " ++ show ln ++ ", column: " ++ show cn

main = do
  s <- getContents
  mapM_ print (alexScanTokens s)
}
