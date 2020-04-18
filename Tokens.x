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
    "=="            { tok (\p s -> TokenVerify p)}
    "<="            { tok (\p s -> TokenLessEq p)}  
    ">="            { tok (\p s -> TokenBigEq p)} 
    "!="            { tok (\p s -> TokenNotEq p)}
    while           { tok (\p s -> TokenWhile p)}
    if              { tok (\p s -> TokenIf p)}
    else            { tok (\p s -> TokenElse p)}
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
    TokenVerify AlexPosn        |
    TokenLessEq AlexPosn        |
    TokenNotEq AlexPosn         |
    TokenWhile AlexPosn         |
    TokenIf AlexPosn            |
    TokenElse AlexPosn          |
    TokenTrue AlexPosn          |
    TokenFalse AlexPosn         |
    TokenTypeInt AlexPosn       |
    TokenTypeBool AlexPosn      |
    TokenVar AlexPosn String
    deriving (Eq, Show)

tokenPosn :: Token -> String
tokenPosn (TokenInt (AlexPn a l c) _ ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPlus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMinus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMult (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDiv (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMod (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAnd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOr (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRoundL (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRoundR (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSquareL (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSquareR (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenColon (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSemiColon (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLess (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenBig (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLessEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenBigEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVerify (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNotEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenWhile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIf (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenElse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTrue (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFalse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeInt (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeBool (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar (AlexPn a l c) _ ) = show(l) ++ ":" ++ show(c)

}