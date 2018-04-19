--LEXER--
--IMPORTED MODULES
{ 
module MyLexer (Token(..), AlexPosn(..), alexScanTokens, token_posn) where 
}

--DEFINE METRICS
%wrapper "posn" 
$digit = 0-9      
$alpha = [a-zA-Z]   

--TOKENS DEFINED FOR GRAMMER TO BE USED FOR THE DOMAIN SPECIFIC LANGUAGE  

tokens :-
$white+       ; 
"--".*        ; 
choose         { tok (\p s -> TokenChoose p) }
from          { tok (\p s -> TokenFrom p) }
suchThat         { tok (\p s -> TokenSuchThat p) }
and          { tok (\p s -> TokenAnd p) }
aswell        { tok (\p s -> TokenAs p) }
orderBy       { tok (\p s -> TokenOrderBy p) }
dig           { tok (\p s -> TokenInt p (read s)) }
$digit+       { tok (\p s -> TokenInt p (read s)) }
[\=]          { tok (\p s -> TokenEq p) }
[\;]          { tok (\p s -> TokenEOS p) }
[\,]          { tok (\p s -> TokenComma p) }
[\.]          { tok (\p s -> TokenDot p) }
$alpha [$alpha $digit \_ \’]*   { tok (\p s -> TokenVar p s) }

{ 

tok f p s = f p s

-- Token Types Defined (AlexPosn for posn)

data Token = 
     TokenChoose      AlexPosn         |
     TokenFrom        AlexPosn         |
     TokenOrderBy     AlexPosn         |
     TokenSuchThat    AlexPosn         |
     TokenInt         AlexPosn Int     | 
     TokenVar         AlexPosn String  | 
     TokenAnd         AlexPosn         |
     TokenAs          AlexPosn         |
     TokenEq          AlexPosn         |
     TokenComma       AlexPosn         |
     TokenEOS         AlexPosn         |
     TokenDot         AlexPosn
     deriving (Eq,Show) 

--
token_posn (TokenFrom p) = p
token_posn (TokenChoose p) = p
token_posn (TokenSuchThat p) = p
token_posn (TokenOrderBy p) = p
token_posn (TokenAnd p ) = p
token_posn (TokenEq p ) = p
token_posn (TokenComma p ) = p
token_posn (TokenDot p ) = p
token_posn (TokenEOS p ) = p
token_posn (TokenVar p _) = p
token_posn (TokenInt p _ ) = p
}