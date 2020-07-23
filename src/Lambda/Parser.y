{
module Lambda.Parser where

import Lambda.Lex
import Lambda.Syntax
}

%name happyParser
%tokentype { Token }

%monad { Parser } { thenP } { returnP }
%lexer { lexer } { Token _ TokenEOF }

%token
        let             { Token _ TokenLet      }
        in              { Token _ TokenIn       }
        int             { Token _ (TokenInt $$) }
        var             { Token _ (TokenVar $$) }
        '+'             { Token _ TokenPlus     }
        '-'             { Token _ TokenMinus    }
        '*'             { Token _ TokenTimes    }
        '/'             { Token _ TokenDiv      }
        '='             { Token _ TokenEq       }
        '('             { Token _ TokenOB       }
        ')'             { Token _ TokenCB       }
        ','             { Token _ TokenComma    }

%right in
%left '+' '-'
%right '*' '/'
%left NEG

%%

Exp :: {Exp}
     : let Bindings in Exp	{ Let (tokenToPosN $1) $2 $4 }
     | Exp '+' Exp              { Plus $1 $3 }
     | Exp '-' Exp              { Minus $1 $3 }
     | Exp '*' Exp              { Times $1 $3 }
     | Exp '/' Exp              { Div $1 $3 }
     | '(' Exp ')'              { Brack $2 }
     | '-' Exp %prec NEG        { Negate $2 }
     | int                      { Int $1 }
     | var                      { Var $1 }

Binding :: {Binding}
         : var '=' Exp          { Bind $1 $3 }

Bindings :: {[Binding]}
          : Binding             { [$1] }
          | Bindings ',' Binding    { $3 : $1 }

{}
