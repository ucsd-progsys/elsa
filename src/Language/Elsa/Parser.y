{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Language.Nano.Parser (
    parseExpr
  , parseTokens
  ) where

import Language.Nano.Lexer
import Language.Nano.Types hiding (Nano (..))
import Control.Monad.Except
import Control.Exception

}

-- Entry point
%name top

-- Lexer structure
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    let   { LET _    }
    true  { TRUE _   }
    false { FALSE _  }
    in    { IN _     }
    if    { IF _     }
    then  { THEN _   }
    else  { ELSE _   }
    TNUM  { NUM _ $$ }
    ID    { ID _ $$  }
    '\\'  { LAM _    }
    '->'  { ARROW _  }
    '='   { EQB _    }
    '+'   { PLUS _   }
    '-'   { MINUS _  }
    '*'   { MUL _    }
    '&&'  { AND _    }
    '||'  { OR  _    }
    '=='  { EQL _    }
    '/='  { NEQ _    }
    '<'   { LESS _   }
    '<='  { LEQ _    }
    ':'   { COLON _  }
    '('   { LPAREN _ }
    ')'   { RPAREN _ }
    '['   { LBRAC _  }
    ']'   { RBRAC _  }
    ','   { COMMA _  }


-- Operators
%right in
%nonassoc '=' '==' '/=' '<' '<=' if then else
%right ':' '->'
%left '||' '&&'
%left '+' '-'
%left '*'
%%

Top  : ID '=' Expr                 { $3 }
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
mkLam :: [Id] -> Expr -> Expr
mkLam []     e = e
mkLam (x:xs) e = ELam x (mkLam xs e)

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
