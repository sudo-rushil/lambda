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
        ln              { Token _ TokenLine     }

-- %right '(' '['
%left '.'
%left var lam
%right '(' '['
%left APP
-- %right '(' '['


%%

File :: {[Stmt]}
     : {-- empty --}                    { [] }
     | File ln Stmt                     { $3 : $1 }
     | File ln                          { $1 }          -- handle trailing lines

Stmt :: {Stmt}
     : let var '=' Expr                 { Bind $2 $4 }
     | Expr                             { Exp $1 }

Expr :: {Expr}
     : lam Vars '.' Expr                { expandBindings (reverse $2) $4 }
     | Expr Expr %prec APP              { App $1 $2 }
     | var                              { Var $1 }
     | '(' Expr ')'                     { $2 }
     | '[' Expr ']'                     { $2 }

Vars :: {[Name]}
     : var                              { [$1] }
     | Vars var                         { $2 : $1 }


{

-- Expansion of multiple lambda binding syntactic sugar
expandBindings :: [Name] -> Expr -> Expr
expandBindings [name] expr       = Abs name expr
expandBindings (name:names) expr = Abs name (expandBindings names expr)

}
