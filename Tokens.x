{
module Tokens where
}

%wrapper "posn"

$digit = 0-9

$alpha = [a-zA-Z]

tokens :-
    $white+          ;
    "--".*           ;
    Bool             { tok ( \p s -> TokenTypeBool p) }
    Int              { tok ( \p s -> TokenTypeInt p) }
    $digit+          { tok ( \p s -> TokenInt p (read s)) }
    \=               { tok ( \p s -> TokenEq p) }
    \!=              { tok ( \p s -> TokenNotEq p) }
    \+               { tok ( \p s -> TokenPlus p) }
    \-               { tok ( \p s -> TokenMinus p) }
    \*               { tok ( \p s -> TokenMult p) }
    \/               { tok ( \p s -> TokenDiv p) }
    \%               { tok ( \p s -> TokenMod p) }
    \<               { tok ( \p s -> TokenLessThan p) }
    \<=              { tok ( \p s -> TokenLessOrEqThan p) }
    \>               { tok ( \p s -> TokenBiggerThan p) }
    \>=              { tok ( \p s -> TokenBiggerOrEqThan p) }
    \==              { tok ( \p s -> TokenIsEq p) }
    \&&              { tok ( \p s -> TokenAnd p) }
    "||"             { tok ( \p s -> TokenOr p) }
    if               { tok ( \p s -> TokenIf p) }
    else             { tok ( \p s -> TokenElse p) }
    while            { tok ( \p s -> TokenWhile p) }
    true             { tok ( \p s -> TokenTrue p) }
    false            { tok ( \p s -> TokenFalse p) }
    \;               { tok ( \p s -> TokenSemiCol p) }
    \,               { tok ( \p s -> TokenComma p)}
    \(               { tok ( \p s -> TokenLRoundB p) }
    \)               { tok ( \p s -> TokenRRoundB p) }
    \{               { tok ( \p s -> TokenLCurlyB p) }
    \}               { tok ( \p s -> TokenRCurlyB p) }
    \[               { tok ( \p s -> TokenLSquareB p) }
    \]               { tok ( \p s -> TokenRSquareB p) }
    \:               { tok ( \p s -> TokenColon p) }
    "/*"             { tok ( \p s -> TokenCommentL p) }
    "*/"             { tok ( \p s -> TokenCommentR p) }
    "-="             { tok ( \p s -> TokenMinusMany p) }
    "+="             { tok ( \p s -> TokenAddMany p) }
    ReadStream       { tok ( \p s -> TokenReadStream p) }
    sizeOf           { tok ( \p s -> TokenSizeOf p) }
    output           { tok ( \p s -> TokenOutput p) }
    forEach          { tok ( \p s -> TokenForEach p) } 
    $alpha [$alpha $digit \_ \']*        { tok (\p s -> TokenVar p s) }
    $alpha [$alpha $digit \_ \']*        { tok (\p s -> TokenString p s) }
   
{

--helper function
tok f p s = f p s

data Token =
    TokenTypeBool AlexPosn          |
    TokenTypeInt AlexPosn           |
    TokenInt AlexPosn Int           |
    TokenEq AlexPosn                |
    TokenPlus AlexPosn              |
    TokenMinus AlexPosn             |
    TokenMult AlexPosn              |
    TokenDiv AlexPosn               |
    TokenMod AlexPosn               |
    TokenLessThan AlexPosn          |
    TokenLessOrEqThan AlexPosn      |
    TokenBiggerThan AlexPosn        |
    TokenBiggerOrEqThan AlexPosn    |
    TokenIsEq AlexPosn              |
    TokenNotEq AlexPosn             |
    TokenAnd AlexPosn               |
    TokenOr AlexPosn                |
    TokenIf AlexPosn                |
    TokenElse AlexPosn              |
    TokenWhile AlexPosn             |
    TokenTrue AlexPosn              |
    TokenFalse AlexPosn             |
    TokenSemiCol AlexPosn           |
    TokenComma AlexPosn             | 
    TokenLRoundB AlexPosn           |
    TokenRRoundB AlexPosn           |
    TokenLCurlyB AlexPosn           |
    TokenRCurlyB AlexPosn           |
    TokenLSquareB AlexPosn          |
    TokenRSquareB AlexPosn          |
    TokenVar AlexPosn String        |
    TokenString AlexPosn String     |
    TokenReadStream AlexPosn        |
    TokenSizeOf AlexPosn            |
    TokenForEach AlexPosn           |
    TokenColon AlexPosn             |
    TokenCommentL AlexPosn          |
    TokenCommentR AlexPosn          |
    TokenAddMany AlexPosn           |
    TokenMinusMany AlexPosn         |
    TokenOutput AlexPosn
    deriving (Eq, Show)

tokenPosn :: Token -> String
tokenPosn (TokenTypeBool (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeInt (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInt (AlexPn a l c) _ ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPlus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMinus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMult (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDiv (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMod (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLessThan (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLessOrEqThan (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenBiggerThan (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenBiggerOrEqThan (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIsEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNotEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAnd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOr (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIf (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenElse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenWhile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTrue (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFalse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSemiCol (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenComma (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLRoundB (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRRoundB (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLCurlyB (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRCurlyB (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLSquareB (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRSquareB (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenReadStream (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSizeOf (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenForEach (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenColon (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenCommentL (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenCommentR (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAddMany (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMinusMany (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOutput (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar (AlexPn a l c) _ ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenString (AlexPn a l c) _ ) = show(l) ++ ":" ++ show(c)
}