{
    module Grammar where
    import Tokens
}

%name parseCalc
%tokentype { Token }
%error { parseError }

-- Tokens
%token
    digit { TokenInt _ $$ }
    '+' { TokenPlus _ }
    '-' { TokenMinus _ }
    '*' { TokenMult _ }
    '/' { TokenDiv _ }
    '%' { TokenMod _ }
    and { TokenAnd _ }
    or { TokenOr _ }
    '(' { TokenRoundL _ }
    ')' { TokenRoundR _ }
    '[' { TokenSquareL _ }
    ']' { TokenSquareR _ }
    '{' { TokenCurlyL _ }
    '}' { TokenCurlyR _ }
    ':' { TokenColon _ }
    ';' { TokenSemiColon _ }
    '<' { TokenLess _ }
    '>' { TokenBig _ }
    '=' { TokenEq _ }
    '==' { TokenVerify _ }
    lessEq { TokenLessEq _ }
    bigEq { TokenBigEq _ }
    notEq { TokenNotEq _ }
    while { TokenWhile _ }
    if { TokenIf _ }
    else { TokenElse _ }
    true { TokenTrue _ }
    false { TokenTrue _ }
    Int { TokenTypeInt _ }
    Bool { TokenTypeBool _ }
    var { TokenVar _ $$ }

%%

Exp : if '(' ShortExp ')' '{' Exp '}' else '{' Exp '}'    { If $3 $6 $10 }
    | while '(' ShortExp ')' '{' Exp '}'                  { While $3 $6 }
    | var '=' ShortExp ';'                                 { Assignment $1 $3 }
    | Exp Exp                                               { $1 $2 }
    | MathExp '=' MathExp ';'                               { Operation $1 $3 }

MathExp : MathExp '+' MathExp                              { Plus $1 $3 }
    | MathExp '-' MathExp                              { Minus $1 $3 }
    | MathExp '/' MathExp                              { Divide $1 $3 }
    | MathExp '*' MathExp                              { Multiply $1 $3 }
    | MathExp '%' MathExp                              { Modulo $1 $3 }
    | '(' MathExp ')'                                   { $1 }
    | int                                           { Int $1 }
    | var                                           { Var $1 }

ShortExp : ShortExp '&&' ShortExp                         { And $1 $3 }
    | ShortExp '||' ShortExp                              { Or $1 $3 }
    | ShortExp '<' ShortExp                         { LessThen $1 $3 }
    | ShortExp '>' ShortExp                         { BiggerThen $1 $3 }
    | ShortExp '==' ShortExp                         { Equal $1 $3 }
    | ShortExp '<=' ShortExp                         { LessOrEqualThen $1 $3 }
    | ShortExp '>=' ShortExp                         { BiggerOrEqualThen $1 $3 }
    | ShortExp '!=' ShortExp                         { NotEqual $1 $3 }
    | ShortExp '+' ShortExp                              { Plus $1 $3 }
    | ShortExp '-' ShortExp                              { Minus $1 $3 }
    | ShortExp '/' ShortExp                              { Divide $1 $3 }
    | ShortExp '*' ShortExp                              { Multiply $1 $3 }
    | ShortExp '%' ShortExp                              { Modulo $1 $3 }
    | '(' ShortExp ')'                                   { $1 }
    | true                                          { BoolTrue }
    | false                                         { BoolFalse }
    | int                                           { Int $1 }
    | var                                           { Var $1 }

{
    data 
}