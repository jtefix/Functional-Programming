{
module Grammar where
import Tokens
}

%name parseCalc
%tokentype { Token }
%error { parseError }

--Tokens
%token
    Bool    { TokenTypeBool _ }
    Int     { TokenTypeInt _ }
    int     { TokenInt _ $$ }
    '='     { TokenEq _ }
    '!='    { TokenNotEq _ }
    '+'     { TokenPlus _ }
    '-'     { TokenMinus _ }
    '*'     { TokenMult _ }
    '/'     { TokenDiv _ }
    '%'     { TokenMod _ }
    '<'     { TokenLessThan _ }
    '<='    { TokenLessOrEqThan _ }
    '>'     { TokenBiggerThan _ }
    '>='    { TokenBiggerOrEqThan _ }
    '=='    { TokenIsEq _ }
    '&&'    { TokenAnd _ }
    '||'    { TokenOr _ }
    if      { TokenIf _ }
    else    { TokenElse _ }
    while   { TokenWhile _ }
    true    { TokenTrue _ }
    false   { TokenFalse _ }
    ';'     { TokenSemiCol _ }
    '('     { TokenLRoundB _ }
    ')'     { TokenRRoundB _ }
    '{'     { TokenLCurlyB _ }
    '}'     { TokenRCurlyB _ } 
    var     { TokenVar _ $$ }

%nonassoc if
%nonassoc else
%nonassoc while
%nonassoc int var true false
%nonassoc '(' ')' '{' '}'
%left '<' '<=' '>' '>=' '==' '!=' '&&' '||'
%left '+' '-'
%left '*' '/' '%'
%right '='
%left APP
%%

Exp : if '(' ShortExp ')' '{' Exp '}'                         { IfStmt $3 $6 }
    | if '(' ShortExp ')' '{' Exp '}' else '{' Exp '}'   { IfElseStmt $3 $6 $10 }
    | while '(' ShortExp ')' '{' Exp '}'                      { WhileExp $3 $6 }
    | var '=' MathExp ';'                                          { Assignment $1 $3 }
    | Exp Exp %prec APP                                     { App $1 $2 }
   
ShortExp : MathExp '<' MathExp                                     { LessThan $1 $3 }  
         | MathExp '<=' MathExp                                    { LessOrEqThan $1 $3 }
         | MathExp '>' MathExp                                     { BiggerThan $1 $3 }
         | MathExp '>=' MathExp                                    { BiggerOrEqThan $1 $3 }
         | ShortExp '==' ShortExp                                  { IsEq $1 $3 }
         | ShortExp '!=' ShortExp                                  { NotEq $1 $3 }
         | ShortExp '&&' ShortExp                                  { And $1 $3 }
         | ShortExp '||' ShortExp                                  { Or $1 $3 }
         | true                                                    { LanTrue }
         | false                                                   { LanFalse }

MathExp : MathExp '+' MathExp                                      { Plus $1 $3 }
        | MathExp '-' MathExp                                      { Minus $1 $3 }
        | MathExp '*' MathExp                                      { Mult $1 $3 }
        | MathExp '/' MathExp                                      { Div $1 $3 }
        | MathExp '%' MathExp                                      { Mod $1 $3 }
        | '(' Exp ')'                                              { $2 }
        | int                                                      { LanInt $1 }
        | var                                                      { LanVar $1 }

Type : Bool { TypeBool }
     | Int { TypeInt }

{
-- error function
parseError :: [Token] -> a
parseError [] = error "Unknown parse error"
parseError (t:_) = error ("Parse error at line:column " ++ ( tokenPosn t))

data Exp = App Exp Exp
         | Assignment String Exp
         | Plus Exp Exp
         | Minus Exp Exp
         | Mult Exp Exp
         | Div Exp Exp
         | Mod Exp Exp
         | LessThan Exp Exp
         | LessOrEqThan Exp Exp
         | BiggerThan Exp Exp
         | BiggerOrEqThan Exp Exp
         | IsEq Exp Exp
         | NotEq Exp Exp
         | And Exp Exp
         | Or Exp Exp
         | LanTrue
         | LanFalse
         | LanInt Int
         | LanVar String
         | IfStmt Exp Exp
         | IfElseStmt Exp Exp Exp
         | WhileExp Exp Exp
    deriving (Show, Eq)


data Type = TypeInt | TypeBool 
      deriving (Show, Eq)

--type Environment = [ (String, StmtList) ]      


}