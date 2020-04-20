{
module Grammar where
import Tokens
}

%name parseCalc
%tokentype { Token }
%error { parseError }

-- Tokens
%token
    --digit { TokenInt _ $$ }
    '+' { TokenPlus _ }
    '-' { TokenMinus _ }
    '*' { TokenMult _ }
    '/' { TokenDiv _ }
    '%' { TokenMod _ }
    '&&' { TokenAnd _ }
    '||' { TokenOr _ }
    '(' { TokenRoundL _ }
    ')' { TokenRoundR _ }
    --'[' { TokenSquareL _ }
    --']' { TokenSquareR _ }
    '{' { TokenCurlyL _ }
    '}' { TokenCurlyR _ }
    --':' { TokenColon _ }
    ';' { TokenSemiColon _ }
    '<' { TokenLess _ }
    '>' { TokenBig _ }
    '=' { TokenEq _ }
    '==' { TokenVerify _ }
    '<=' { TokenLessEq _ }
    '>=' { TokenBigEq _ }
    '!=' { TokenNotEq _ }
    while { TokenWhile _ }
    if { TokenIf _ }
    else { TokenElse _ }
    true { TokenTrue _ }
    false { TokenTrue _ }
    Int { TokenTypeInt _ }
    -- Bool { TokenTypeBool _ }
    var { TokenVar _ $$ }

%nonassoc if 
%nonassoc else
%nonassoc while
%nonassoc Int var --digit
%nonassoc true false --Bool
%nonassoc ';' --':'
%nonassoc '(' ')'  '{' '}' --'[' ']'
%right '='
%left '&&' '||'
%left '==' '!='
%left '<' '>' '<=' '>=' 
%left '+' '-'
%left '*' '/' '%'
--%left NEG
%%

Exp : if '(' ShortExp ')' '{' Exp '}' else '{' Exp '}'    { If $3 $6 $10 }
    | while '(' ShortExp ')' '{' Exp '}'             { While $3 $6 }
    | var '=' Exp ';'                                { Assignment $1 $3 }
    | Exp '+' Exp                                    { Plus $1 $3 }
    | Exp '-' Exp                                    { Minus $1 $3 }
    | Exp '/' Exp                                    { Divide $1 $3 }
    | Exp '*' Exp                                    { Multiply $1 $3 }
    | Exp '%' Exp                                    { Modulo $1 $3 }
    | '(' Exp ')'                                    { $1 }
    | Int                                            { Int $1 }
    | var                                            { Var $1 }
    | true                                           { BoolTrue }
    | false                                          { BoolFalse }

ShortExp : ShortExp '&&' ShortExp                    { And $1 $3 }
    | ShortExp '||' ShortExp                         { Or $1 $3 }
    | ShortExp '<' ShortExp                          { LessThen $1 $3 }
    | ShortExp '>' ShortExp                          { BiggerThen $1 $3 }
    | ShortExp '==' ShortExp                         { Equal $1 $3 }
    | ShortExp '<=' ShortExp                         { LessOrEqualThen $1 $3 }
    | ShortExp '>=' ShortExp                         { BiggerOrEqualThen $1 $3 }
    | ShortExp '!=' ShortExp                         { NotEqual $1 $3 }
    | ShortExp '+' ShortExp                          { Plus $1 $3 }
    | ShortExp '-' ShortExp                          { Minus $1 $3 }
    | ShortExp '/' ShortExp                          { Divide $1 $3 }
    | ShortExp '*' ShortExp                          { Multiply $1 $3 }
    | ShortExp '%' ShortExp                          { Modulo $1 $3 }
    | '(' ShortExp ')'                               { $1 }
    | true                                           { BoolTrue }
    | false                                          { BoolFalse }
    | Int                                            { Int $1 }
    | var                                            { Var $1 }


{
-- error function
parseError :: [Token] -> a
parseError [] = error "Unknown parse error"
parseError (t:_) = error ("Parse error at " ++ (show $ tokenPosn t))

data Exp = If Exp Exp Exp
         | While Exp Exp
         | Assignment Exp Exp
         | Plus Exp Exp
         | Minus Exp Exp
         | Multiply Exp Exp
         | Divide Exp Exp
         | Modulo Exp Exp
         | BoolTrue
         | BoolFalse
         | And Exp Exp
         | Or Exp Exp
         | LessThen Exp Exp
         | BiggerThen Exp Exp
         | LessOrEqualThen Exp Exp
         | BiggerOrEqualThen Exp Exp
         | NotEqual Exp Exp
         | Integer Int
         | Var String
        deriving (Show, Eq)
}