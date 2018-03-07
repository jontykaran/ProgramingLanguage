
{ 
module Main(main) where 
}

%wrapper "basic" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]  
$eol = [\n]  
-- alphabetic characters

tokens :-
$white+       ; 
  "--".*        ; 
  let           { \s -> TokenLet } 
  in            { \s -> TokenIn }
  True          {\s -> TokenTrue}
  False         {\s -> TokenFalse}
  "**"          { \s -> TokenStartComm}
  "*/"          { \s -> TokenEndComm}
  $digit+       { \s -> TokenInt (read s) } 
  [\=]          { \s -> TokenEq }
  [\~]          { \s -> TokenNotEq }
  [\+]          { \s -> TokenPlus }
  [\-]          { \s -> TokenMinus }
  [\*]          { \s -> TokenTimes }
  [\;]          { \s -> TokenEOS }
  [\^]          { \s -> TokenAnd }
  [\&&]         { \s -> TokenAnd }
  [\|]        { \s -> TokenOR }
  [\,]        { \s -> TokenComma}
  \/          { \s -> TokenDiv }
  \(          { \s -> TokenLParen }
  \)          { \s -> TokenRParen }

  $alpha [$alpha $digit \_ \’]*   { \s -> TokenVar s } 

{ 
-- Each action has type :: String -> Token 
-- The token type: 
data Token = 
  TokenLet         | 
  TokenTrue        |
  TokenFalse       |
  TokenIn          | 
  TokenInt Int     |
  TokenVar String  | 
  TokenAnd         |
  TokenOR          |
  TokenOr          |
  TokenEq          |
  TokenNotEq       |
  TokenPlus        |
  TokenMinus       |
  TokenTimes       |
  TokenDiv         |
  TokenStartComm   |
  TokenEndComm     |
  TokenComma       |
  TokenEOS         |
  TokenLParen      |
  TokenRParen       
  deriving (Eq,Show) 

scanTokens :: String -> [Token]
scanTokens = alexScanTokens

main = do
 s <- getContents
 print (alexScanTokens s)

}