{
module Main (main) where
}

%wrapper "posn"

$digit = 0-9 -- digits

@identifier = [a-zA-Z_][a-zA-Z_0-9]*
@comment = \{\-.*\-\}

tokens :-

  -- Basics
  $white+ ;
  \{ ; -- TODO
  \} ; -- TODO
  \| ; -- TODO
  @comment { \pos str -> TkComment pos str } -- TODO REMOVE
  @identifier { \pos str -> TkIdentifier pos str } -- TODO CHECK

  -- Constants
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
  \$ ; -- TODO

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

{
-- Each action has type :: PosData -> String -> Token

-- The token type:
data Token =
  TkInteger PosData String |
  TkComment PosData String |
  TkIdentifier PosData String
  deriving (Eq,Show)

type PosData = AlexPosn

main = do
  s <- getContents
  print (alexScanTokens s)
}
