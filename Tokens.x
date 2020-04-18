{
    module Tokens where
}

%wrapper "posn"

$digit = 0-9

$alpha = [a-zA-Z]

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
    Int             { tok (\p s -> TokenTypeInt p) }
    Bool            { tok (\p s -> TokenTypeBool p)}
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
    TokenBigEq AlexPosn         |
    TokenLessEq AlexPosn        |
    TokenNotEq AlexPosn         |
    TokenWhile AlexPosn         |
    TokenIf AlexPosn            |
    TokenElse AlexPosn          |
    TokenFor AlexPosn           |
    TokenTrue AlexPosn          |
    TokenFalse AlexPosn         |
    TokenTypeInt AlexPosn       |
    TokenTypeBool AlexPosn      |
    TokenVar AlexPosn String
    deriving (Eq, Show)

tokenPosn :: Token -> AlexPosn
tokenPosn (TokenInt p s ) = p
tokenPosn (TokenPlus p ) = p
tokenPosn (TokenMinus p ) = p
tokenPosn (TokenMult p ) = p
tokenPosn (TokenDiv p ) = p
tokenPosn (TokenMod p ) = p
tokenPosn (TokenAnd p ) = p
tokenPosn (TokenOr p ) = p
tokenPosn (TokenRoundL p ) = p
tokenPosn (TokenRoundR p ) = p
tokenPosn (TokenSquareL p ) = p
tokenPosn (TokenSquareR p ) = p
tokenPosn (TokenColon p ) = p
tokenPosn (TokenSemiColon p ) = p
tokenPosn (TokenLess p ) = p
tokenPosn (TokenBig p ) = p
tokenPosn (TokenLessEq p ) = p
tokenPosn (TokenBigEq p ) = p
tokenPosn (TokenEq p ) = p
tokenPosn (TokenNotEq p ) = p
tokenPosn (TokenWhile p ) = p
tokenPosn (TokenIf p ) = p
tokenPosn (TokenElse p ) = p
tokenPosn (TokenFor p ) = p
tokenPosn (TokenTrue p ) = p
tokenPosn (TokenFalse p ) = p
tokenPosn (TokenTypeInt p ) = p
tokenPosn (TokenTypeBool p ) = p
tokenPosn (TokenVar p s ) = p

}