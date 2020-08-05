{

{-# LANGUAGE OverloadedStrings                 #-}
{-# LANGUAGE NoMonomorphismRestriction          #-}
{-# LANGUAGE CPP                                #-}
{-# OPTIONS_GHC -fno-warn-unused-binds          #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures    #-}
{-# OPTIONS_GHC -fno-warn-unused-matches        #-}
{-# OPTIONS_GHC -fno-warn-unused-imports        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing        #-}
{-# OPTIONS_GHC -fno-warn-tabs                  #-}
{-# OPTIONS_GHC -funbox-strict-fields           #-}

module Lambda.Lexer
  ( Alex(..)
  , AlexPosn(..)
  , AlexState(..)
  , Token(..)
  , TokenClass(..)
  , alexError
  , alexMonadScan
  , runAlex
  , tokenToPosN
  )
where

import System.Exit
import qualified Data.ByteString.Lazy.Char8 as B
}

%wrapper "monadUserState-bytestring"

$digit = 0-9                    -- digits
$alpha = [a-zA-Z]
$idens = $printable # [\\=\(\)\[\]\.\ \t\n\f\v\r]


tokens :-

  [\n]+                                 { tok          TokenLine }
  $white+                               ;
  "--".*                                ;
  let                                   { tok          TokenLet }
  use                                   { tok          TokenUse }
  :$idens+                              { tok_string   TokenCmd }
  [=]                                   { tok          TokenEq }
  [\\]                                  { tok          TokenLam }
  [\(]                                  { tok          TokenOP }
  [\)]                                  { tok          TokenCP }
  [\[]                                  { tok          TokenOB }
  [\]]                                  { tok          TokenCB }
  [\.]                                  { tok          TokenDot }
  $idens+                               { tok_string   TokenVar }

{

-- Some action helpers:
tok' f (p, _, input, _) len = return $ Token p (f (B.take (fromIntegral len) input))
tok x = tok' (\s -> x)
tok_string x = tok' (\s -> x (B.unpack s))

-- The token type:
data Token = Token AlexPosn TokenClass
  deriving (Show)

tokenToPosN :: Token -> AlexPosn
tokenToPosN (Token p _) = p

data TokenClass
 = TokenLine
 | TokenLet
 | TokenUse
 | TokenCmd    String
 | TokenVar    String
 | TokenEq
 | TokenLam
 | TokenOP
 | TokenCP
 | TokenOB
 | TokenCB
 | TokenDot
 | TokenEOF
 deriving (Eq, Show)

alexEOF :: Alex Token
alexEOF = do
  (p, _, _, _) <- alexGetInput
  return $ Token p TokenEOF

type AlexUserState = ()
alexInitUserState = ()
}
