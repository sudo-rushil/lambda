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

%left '.'
%left '_'

%%

Stmt :: {Stmt}
     : let var '=' Expr                 { Bind (Var $2) $4 }
     | Expr                             { Exp $1 }

Expr :: {Expr}
     : var                              { Var $1 }
     | lam var '.' Expr                 { Abs $2 $4 }
     | '(' Expr ')'                     { $2 }
     | '[' Expr ']'                     { $2 }
     | Expr Expr                        { App $1 $2 }


{}
