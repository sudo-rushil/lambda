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
        var             { Token _ (TokenVar $$) }
        '='             { Token _ TokenEq       }
        lam             { Token _ TokenLam      }
        '('             { Token _ TokenOP       }
        ')'             { Token _ TokenCP       }
        '['             { Token _ TokenOB       }
        ']'             { Token _ TokenCB       }
        '.'             { Token _ TokenDot      }

-- %right '(' '['
%left '.'
%left var lam
%right '(' '['
%left APP
-- %right '(' '['


%%

Stmt :: {Stmt}
     : let var '=' Expr                 { Bind (Var $2) $4 }
     | Expr                             { Exp $1 }

Expr :: {Expr}
     : lam var '.' Expr                 { Abs $2 $4 }
     | Expr Expr %prec APP              { App $1 $2 }
     | var                              { Var $1 }
     | '(' Expr ')'                     { $2 }
     | '[' Expr ']'                     { $2 }



{}
