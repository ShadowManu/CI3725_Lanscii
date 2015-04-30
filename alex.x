{
module Main (main) where
}

%wrapper "posn"

$digit = 0-9 -- digits

@identifier = [a-zA-Z_][a-zA-Z_0-9]*
@comment = \{\-.*\-\}

tokens :-

	$white+ ;

	!@identifier { \pos str -> TkInteger pos str } -- TODO PENDING

	@identifier { \pos str -> TkIdentifier pos str } -- TODO CHECK
	@comment { \pos str -> TkComment pos str } -- TODO REMOVE

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
