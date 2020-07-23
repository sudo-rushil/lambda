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
        '_'             { Token _ TokenSpace    }
        let             { Token _ TokenLet      }
        var             { Token _ (TokenVar $$) }
        '='             { Token _ TokenEq       }
        lam             { Token _ TokenLam      }
        '('             { Token _ TokenOP       }
        ')'             { Token _ TokenCP       }
        '['             { Token _ TokenOB       }
        ']'             { Token _ TokenCB       }
        '.'             { Token _ TokenDot      }

%left '_' '.'

%%

Stmt :: {Stmt}
     : let var '=' Expr          { Bind $2 $4 }
     | Expr                     { Exp $1 }

Expr :: {Expr}
     : lam var '.' Expr          { Abs $2 $4 }
     | Expr '_' Expr              { App $1 $3 }
     | '(' Expr ')'              { Brack $2 }
     | '[' Expr ']'              { Brack $2 }
     | var                      { Var $1 }


{}
