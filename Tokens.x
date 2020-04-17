{
    module Tokens where
}

%wrapper "posn"

%digit = 0-9

%alpha = [a-zA-Z]

tokens :-
    $white+         ;
    "--".*          ;
    $digit+         { tok (\p s -> TokenInt p (read s)) }
    \+              { tok (\p s -> TokenPlus p)}
    \-              { tok (\p s -> TokenMinus p)}
    \*              { tok (\p s -> TokenMult p)}
    \/              { tok (\p s -> TokenDiv p)}
    \%              { tok (\p s -> TokenMod p)}
    "&&"            { tok (\p s -> TokenAnd p)}
    "||"            { tok (\p s -> TokenOr p)}
    \(              { tok (\p s -> TokenRoundL p)}
    \)              { tok (\p s -> TokenRoundR p)}
    \[              { tok (\p s -> TokenSquareL p)}
    \]              { tok (\p s -> TokenSquareR p)}
    \{              { tok (\p s -> TokenCurlyL p)}
    \}              { tok (\p s -> TokenCurlyR p)}
    \:              { tok (\p s -> TokenColon p)}
    \;              { tok (\p s -> TokenSemiColon p)}
    \<              { tok (\p s -> TokenLess p)}
    \>              { tok (\p s -> TokenBig p)}
    \=              { tok (\p s -> TokenEq p)}
    "<="            { tok (\p s -> TokenLessEq p)}  
    ">="            { tok (\p s -> TokenBigEq p)} 
    "!="            { tok (\p s -> TokenNotEq p)}
    while           { tok (\p s -> TokenWhile p)}
    if              { tok (\p s -> TokenIf p)}
    else            { tok (\p s -> TokenElse p)}
    for             { tok (\p s -> TokenFor p)}
    true            { tok (\p s -> TokenTrue p)}
    false           { tok (\p s -> TokenFalse p)}
    $alpha [$alpha $digit \_ \']*   {tok (\p s -> TokenVar p s)}

{

-- helper function
tok f p s = f p s

data Token = 
    TokenInt AlexPosn Int       |
    TokenPlus AlexPosn          |
    TokenMinus AlexPosn         |
    TokenMult AlexPosn          |
    TokenDiv AlexPosn           |
    TokenMod AlexPosn           |
    TokenAnd AlexPosn           |
    TokenOr AlexPosn            |
    TokenRoundL AlexPosn        |
    TokenRoundR AlexPosn        |
    TokenSquareL AlexPosn       |
    TokenSquareR AlexPosn       |
    TokenCurlyL AlexPosn        |
    TokenCurlyR AlexPosn        |
    TokenColon AlexPosn         |
    TokenSemiColon AlexPosn     |
    TokenLess AlexPosn          |
    TokenBig AlexPosn           |
    TokenEq AlexPosn            |
    TokenNotEq AlexPosn         |
    TokenWhile AlexPosn         |
    TokenIf AlexPosn            |
    TokenElse AlexPosn          |
    TokenFor AlexPosn           |
    TokenTrue AlexPosn          |
    TokenFalse AlexPosn         |
    TokenVar AlexPosn String
deriving (Show, Eq)

tokenPosn :: Token -> String
tokenPosn (TokenInt s p) = p
tokenPosn (TokenPlus )
tokenPosn (TokenMinus)
tokenPosn (TokenMult)
tokenPosn (TokenDiv )
tokenPosn (TokenMod )
tokenPosn (TokenAnd )
tokenPosn (TokenOr )
tokenPosn (TokenRoundL)
tokenPosn (TokenRoundR )
tokenPosn (TokenSquareL )
tokenPosn (TokenSquareR )
tokenPosn ()
tokenPosn ()
tokenPosn ()
tokenPosn ()
tokenPosn ()
tokenPosn ()
tokenPosn 
tokenPosn 
tokenPosn 
tokenPosn 
tokenPosn 
tokenPosn 

}