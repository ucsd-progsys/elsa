{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Elsa.Types where

import           Text.Printf (printf)
import           Data.Monoid
import           Language.Elsa.UX

type Id    = String

type SElsa = Elsa SourceSpan
type SDefn = Defn SourceSpan
type SExpr = Expr SourceSpan
type SEval = Eval SourceSpan
type SStep = Step SourceSpan
type SBind = Bind SourceSpan
type SEqn  = Eqn  SourceSpan

--------------------------------------------------------------------------------
-- | Result
--------------------------------------------------------------------------------
data Result a
  = OK
  | Partial a
  | Invalid a
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Programs
--------------------------------------------------------------------------------

data Elsa a = Elsa
  { defns :: [Defn a]
  , evals :: [Eval a]
  }
  deriving (Eq, Show)

data Defn a
  = Defn !(Bind a) !(Expr a)
  deriving (Eq, Show)

data Eval a = Eval
  { evName  :: !(Bind a)
  , evRoot  :: !(Expr a)
  , evSteps :: [Step a]
  }
  deriving (Eq, Show)

data Step a
  = Step !(Eqn a) !(Expr a)
  deriving (Eq, Show)

data Eqn a
  = AlphEq a
  | BetaEq a
  | DefnEq a
  deriving (Eq, Show)

data Bind a
  = Bind Id a
  deriving (Eq, Show)

data Expr a
  = EVar Id                  a
  | ELam !(Bind a) !(Expr a) a
  | EApp !(Expr a) !(Expr a) a
  deriving (Eq, Show)

-------------------------------------------------------------------------------------
-- | Pretty Printing
-------------------------------------------------------------------------------------
instance PPrint (Bind a) where
  pprint (Bind x _) = x

instance PPrint [Bind a] where
  pprint = unwords . map pprint

instance PPrint (Expr a) where
  pprint (EVar x _)     = x
  pprint (EApp e1 e2 _) = printf "(%s %s)" (pprint e1) (pprint e2)
  pprint e@(ELam {})    = printf "\\%s -> %s" (pprint xs) (pprint body)
    where
      (xs, body)        = bkLam e

bkLam :: Expr a -> ([Bind a], Expr a)
bkLam (ELam x e _) = (x:xs, body)
  where
    (xs, body)     = bkLam e
bkLam e            = ([], e)

mkLam :: (Monoid a) => [Bind a] -> Expr a -> Expr a
mkLam []       e = e
mkLam (x:xs) e = ELam x (mkLam xs e) (tag x <> tag e)

-------------------------------------------------------------------------------------
-- | Tag Extraction
-------------------------------------------------------------------------------------

class Tagged t where
  tag :: t a -> a

instance Tagged Bind where
  tag (Bind _   x) = x

instance Tagged Expr where
  tag (EVar _   x) = x
  tag (ELam _ _ x) = x
  tag (EApp _ _ x) = x
