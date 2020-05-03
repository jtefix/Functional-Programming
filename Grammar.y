{
module Grammar where
import Tokens
}

%name parseCalc
%tokentype { Token }
%error { parseError }

--Tokens
%token
    Bool        { TokenTypeBool _ }
    Int         { TokenTypeInt _ }
    int         { TokenInt _ $$ }
    '='         { TokenEq _ }
    '!='        { TokenNotEq _ }
    '+'         { TokenPlus _ }
    '-'         { TokenMinus _ }
    '*'         { TokenMult _ }
    '/'         { TokenDiv _ }
    '%'         { TokenMod _ }
    '<'         { TokenLessThan _ }
    '<='        { TokenLessOrEqThan _ }
    '>'         { TokenBiggerThan _ }
    '>='        { TokenBiggerOrEqThan _ }
    '=='        { TokenIsEq _ }
    '&&'        { TokenAnd _ }
    '||'        { TokenOr _ }
    if          { TokenIf _ }
    else        { TokenElse _ }
    while       { TokenWhile _ }
    true        { TokenTrue _ }
    false       { TokenFalse _ }
    ';'         { TokenSemiCol _ }
    ','         { TokenComma _ }
    '('         { TokenLRoundB _ }
    ')'         { TokenRRoundB _ }
    '{'         { TokenLCurlyB _ }
    '}'         { TokenRCurlyB _ }
    '['         { TokenLSquareB _ }
    ']'         { TokenRSquareB _ }
    '/*'        { TokenCommentL _ }
    '*/'        { TokenCommentR _ }
    ':'         { TokenColon _ }
    '^'         { TokenPow _ }
    '-='        { TokenMinusMany _ }
    '+='        { TokenAddMany _ }
    forEach     { TokenForEach _}
    var         { TokenVar _ $$ }
    ReadStream  { TokenReadStream _ }
    sizeOf      { TokenSizeOf _ }
    output      { TokenOutput _ }

%nonassoc if 
%nonassoc else
%nonassoc while
%nonassoc int var true false forEach 
%nonassoc ReadStream SizeOf output
%nonassoc '(' ')' '{' '}' '[' ']' ':'
%nonassoc '/*' '*/'
%nonassoc Bool Int ';' ','
%left '<' '<=' '>' '>=' '==' '!=' '&&' '||' 
%left '+' '-' '-=' '+='
%left '*' '/' '%' '^'
%right '=' 
%left NEG 
%right APP 
%%

Exp : if '(' ShortExp ')' '{' Exp '}'                              { IfStmt $3 $6 }
    | if '(' ShortExp ')' '{' Exp '}' else '{' Exp '}'             { IfElseStmt $3 $6 $10 }
    | while '(' ShortExp ')' '{' Exp '}'                           { WhileExp $3 $6 }
    | forEach '(' var ':' var '[' ']' ')' '{' Exp '}'              { ForEach $3 $5 $10 }
    | Exp Exp %prec APP                                            { App $1 $2 }
    | '/*' var '*/'                                                { Comment $2 }
    | NewType ';'                                                  { $1 } 
    | var '=' MathExp ';'                                          { Assignment $1 $3 }
    | var '=' BoolValues ';'                                       { Assignment $1 $3 }
    | var '[' ']' '=' ReadStream ';'                               { StreamRead $1 }
    | var '[' ']' '=' '[' list ']' ';'                             { Assignment $1 $6 }
    | var '[' ']' '=' EmptyList ';'                                { Assignment $1 $5 }
    | var '[' MathExp ']' '=' MathExp ';'                          { IndexAssignment $1 $3 $6 }
    | output '(' MathExp ')' ';'                                   { Output $3 }
    | var '+''+' ';'                                               { AddOne $1 }
    | var '+=' MathExp ';'                                         { AddMany $1 $3}
    | var '-=' MathExp ';'                                         { MinusMany $1 $3}
    | var '[' MathExp ']' '-=' MathExp ';'                         { MinusManyIndexOf $1 $3 $6}
    | var '[' MathExp ']' '+=' MathExp ';'                         { AddManyIndexOf $1 $3 $6}
    | var '[' MathExp ']' '+''+'';'                                { AddOneIndexOf $1 $3 }  

ShortExp : MathExp '<' MathExp                                     { LessThan $1 $3 }  
         | MathExp '<=' MathExp                                    { LessOrEqThan $1 $3 }
         | MathExp '>' MathExp                                     { BiggerThan $1 $3 }
         | MathExp '>=' MathExp                                    { BiggerOrEqThan $1 $3 }
         | MathExp '==' MathExp                                    { IsEq $1 $3 }
         | MathExp '!=' MathExp                                    { NotEq $1 $3 }
         | ShortExp '&&' ShortExp                                  { And $1 $3 }
         | ShortExp '||' ShortExp                                  { Or $1 $3 }
         | '(' ShortExp ')'                                        { $2 }
         | BoolValues                                              { $1 }

BoolValues : true                                                  { LanTrue }
           | false                                                 { LanFalse }

MathExp : MathExp '+' MathExp                                      { Plus $1 $3 }
        | MathExp '-' MathExp                                      { Minus $1 $3 }
        | MathExp '*' MathExp                                      { Mult $1 $3 }
        | MathExp '/' MathExp                                      { Div $1 $3 }
        | MathExp '%' MathExp                                      { Mod $1 $3 }
        | MathExp '^' MathExp                                      { Pow $1 $3 }
        | '-' MathExp %prec NEG                                    { Negate $2 }
        | '(' Exp ')'                                              { $2 }
        | var '[' MathExp ']'                                      { IndexOf $1 $3 }
        | int                                                      { LanInt $1 }
        | var                                                      { LanVar $1 }
        | sizeOf '(' var '[' ']' ')'                               { SizeOf $3 }
  

list : MathExp                                                     { SingleList $1 }
     | MathExp ',' list                                            { MultipleList $1 $3 }

NewType : Type var                                                 { TypeAssignment $1 $2 }
        | Type var '[' ']'                                         { TypeAssignment $1 $2 }

Type : Bool                                                        { TypeBool }
     | Int                                                         { TypeInt }
     | TypeList                                                    { TypeList }

TypeList: Int '[' ']'                                              { TypeList }

EmptyList : '['']'                                                 { EmptyList }

{
-- error function
parseError :: [Token] -> a
parseError [] = error "Unknown parse error"
parseError (t:_) = error ("Parse error at line:column " ++ ( tokenPosn t))

data Exp = App Exp Exp
         | Assignment String Exp
         | TypeAssignment Type String
         | Plus Exp Exp
         | Minus Exp Exp
         | Mult Exp Exp
         | Div Exp Exp
         | Mod Exp Exp
         | Negate Exp
         | Pow Exp Exp
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
         | ForEach String String Exp
         | StreamRead String
         | IndexAssignment String Exp Exp
         | IndexOf String Exp
         | SingleList Exp
         | MultipleList Exp Exp
         | EmptyList
         | SizeOf String
         | Output Exp
         | AddOne String
         | AddMany String Exp
         | MinusMany String Exp
         | MinusManyIndexOf String Exp Exp
         | AddOneIndexOf String Exp
         | AddManyIndexOf String Exp Exp
         | Comment String
    deriving (Show, Eq)

data Type = TypeInt | TypeBool | TypeList
      deriving (Show, Eq)

type Environment = [ (String,Exp) ]  

}