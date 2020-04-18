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
    '+' { TokenPlus  }
    '-' { TokenMinus  }
    '*' { TokenMult }
    '/' { TokenDiv }
    '%' { TokenMod }
    and { TokenAnd  }
    or { TokenOr }
    '(' { TokenRoundL  }
    ')' { TokenRoundR  }
    '[' { TokenSquareL  }
    ']' { TokenSquareR  }
    '{' { TokenCurlyL  }
    '}' { TokenCurlyR  }
    ':' { TokenColon  }
    ';' { TokenSemiColon  }
    '<' { TokenLess  }
    '>' { TokenBig  }
    '=' { TokenEq  }