{ 
module Grammar where 
import Tokens 
}

%name parseCalc 
%tokentype { Token } 
%error { parseError }
%token 
    let { TokenLet _ } 
    in  { TokenIn _} 
    int { TokenInt $$ _ } 
    var { TokenVar $$ _ } 
    '=' { TokenEq _ _ } 
    '+' { TokenPlus _ _ } 
    '-' { TokenMinus _ _} 
    '*' { TokenTimes _ _} 
    '/' { TokenDiv _ _} 
    '(' { TokenLParen _ _} 
    ')' { TokenRParen _ _} 

%right in 
%left '+' '-' 
%left '*' '/' 
%left NEG 
%% 
Exp : let var '=' Exp in Exp { Let $2 $4 $6 } 
    | Exp '+' Exp            { Plus $1 $3 } 
    | Exp '-' Exp            { Minus $1 $3 } 
    | Exp '*' Exp            { Times $1 $3 } 
    | Exp '/' Exp            { Div $1 $3 } 
    | '(' Exp ')'            { $2 } 
    | '-' Exp %prec NEG      { Negate $2 } 
    | int                    { Int $1  } 
    | var                    { Var $1  } 
    
{ 
parseError :: [Token] -> a
parseError tks = error ("Parse error at " ++ lcn ++ "\n")
     where lcn = case tks of
                  [] -> "end of file"
                  tk:_ -> "line " ++ show l ++ ", column " ++ show c
                      where AlexPn _ l c = token_posn tk 
                      
data Exp = Let AlexPosn Exp Exp 
         | Plus Exp Exp 
         | Minus Exp Exp 
         | Times Exp Exp 
         | Div Exp Exp 
         | Negate Exp
         | Int AlexPosn 
         | Var AlexPosn 
         deriving Show 
} 