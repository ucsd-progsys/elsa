{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Language.Elsa.Parser (
    parseExpr
  , parseTokens
  ) where

import Language.Elsa.Lexer
import Language.Elsa.Types  hiding (Nano (..))
import Control.Monad.Except
import Control.Exception

}

-- Entry point
%name elsa

-- Lexer structure
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    let   { LET _    }
    eval  { EVAL _   }
    ID    { ID _ $$  }
    '\\'  { LAM _    }
    '->'  { ARROW _  }
    '=a>' { EQA _    }
    '=b>' { EQA _    }
    '=d>' { EQA _    }
    ':'   { COLON _  }
    '('   { LPAREN _ }
    ')'   { RPAREN _ }

-- Operators
%nonassoc ':'
%right '->'
%%

Elsa  : Defns Evals                 { Elsa $1 $2 }

Defns :                             { []         }
      | Defn Defns                  { $1 : $3    }

Defn  : let Bind '=' Expr           { ($2, $4)   }

Evals :                             { []         }
      | Eval Evals                  { $1 : $3    }

Eval  : eval Bind ':' Expr Steps    { Eval $2 $4 $5 }

Steps :                             { []         }
      | Step Steps                  { $1 : $3    }

Step  : Eqn Expr                    { Step $1 $2 }

Eqn   : '=a>'                       { AlphEq (posnSpan $1) }
      | '=b>'                       { BetaEq (posnSpan $1) }
      | '=d>'                       { DefnEq (posnSpan $1) }

Top   : LET ID '=' Expr Defns       { $3 }
      | Expr                        { $1 }

Expr : Expr ':' Expr                { EBin Cons  $1 $3 }
     | Expr '&&' Expr               { EBin And   $1 $3 }
     | Expr '||' Expr               { EBin Or    $1 $3 }
     | Expr '==' Expr               { EBin Eq    $1 $3 }
     | Expr '/=' Expr               { EBin Ne    $1 $3 }
     | Expr '<'  Expr               { EBin Lt    $1 $3 }
     | Expr '<=' Expr               { EBin Le    $1 $3 }
     | Expr '+'  Expr               { EBin Plus  $1 $3 }
     | Expr '-'  Expr               { EBin Minus $1 $3 }
     | Expr '*'  Expr               { EBin Mul   $1 $3 }
     | if Expr then Expr else Expr  { EIf  $2 $4 $6    }
     | '\\' ID '->' Expr            { ELam $2 $4       }
     | let ID '='  Expr in Expr     { ELet $2 $4 $6    }
     | let ID Ids '=' Expr in Expr  { ELet $2 (mkLam $3 $5) $7 }
     | Axpr                         { $1               }

Axpr : Axpr Bxpr                   { EApp $1 $2       }
     | Bxpr                        { $1               }


Bxpr : TNUM                        { EInt $1        }
     | true                        { EBool True     }
     | false                       { EBool False    }
     | '(' Expr ')'                { $2             }
     | ID                          { EVar $1        }
     | '[' ']'                     { ENil           }
     | '[' Exprs ']'               { exprList $2    }

Exprs : Expr                       { [$1]           }
      | Expr ',' Exprs             { $1 : $3        }

Ids : ID                           { [$1]           }
    | ID Ids                       { $1 : $2        }

{

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError []     = throwError "Unexpected end of Input"

parseExpr :: String -> Expr
parseExpr s = case parseExpr' s of
                Left msg -> throw (Error ("parse error:" ++ msg))
                Right e  -> e

parseExpr' input = runExcept $ do
   tokenStream <- scanTokens input
   top tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens


}
