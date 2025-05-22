{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Elsa.Types where

import           GHC.Generics
import           Text.Printf (printf)
import           Language.Elsa.UX
import           Data.Maybe (mapMaybe)
import           Data.Hashable

type Id        = String
type SElsaItem = ElsaItem SourceSpan
type SElsa     = Elsa SourceSpan
type SDefn     = Defn SourceSpan
type SExpr     = Expr SourceSpan
type SEval     = Eval SourceSpan
type SStep     = Step SourceSpan
type SBind     = Bind SourceSpan
type SEqn      = Eqn  SourceSpan
type SResult   = Result SourceSpan

--------------------------------------------------------------------------------
-- | Result
--------------------------------------------------------------------------------

data Result a
  = OK      (Bind a)
  | Partial (Bind a)    a
  | Invalid (Bind a)    a
  | Unbound (Bind a) Id a
  | DupDefn (Bind a)    a
  | DupEval (Bind a)    a
  deriving (Eq, Show, Functor)

failures :: [Result a] -> [Id]
failures = mapMaybe go
  where
    go (Partial b _)   = Just (bindId b)
    go (Invalid b _)   = Just (bindId b)
    go (Unbound b _ _) = Just (bindId b)
    go (DupDefn b _)   = Just (bindId b)
    go (DupEval b _)   = Just (bindId b)
    go _               = Nothing

successes :: [Result a] -> [Id]
successes = mapMaybe go
  where
    go (OK b) = Just (bindId b)
    go _      = Nothing

resultError :: (Located a) => Result a -> Maybe UserError
resultError (Partial b l)   = mkErr l (bindId b ++ " can be further reduced!")
resultError (Invalid b l)   = mkErr l (bindId b ++ " has an invalid reduction!")
resultError (Unbound b x l) = mkErr l (bindId b ++ " has an unbound variable " ++ x )
resultError (DupDefn b l)   = mkErr l ("Definition " ++ (bindId b) ++ " has already been declared")
resultError (DupEval b l)   = mkErr l ("Evaluation " ++ (bindId b) ++ " has already been declared")
resultError _               = Nothing

mkErr :: (Located a) => a -> Text -> Maybe UserError
mkErr l msg = Just (mkError msg (sourceSpan l))

--------------------------------------------------------------------------------
-- | Programs
--------------------------------------------------------------------------------
data ElsaItem a = DefnItem (Defn a) | EvalItem (Eval a)

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
  = AlphEq a -- Alpha equality
  | BetaEq a -- Single beta reduction
  | UnBeta a
  | EtaaEq a -- Single eta reduction
  | UnEtaa a
  | AtaSEq a -- Alpha equality, is strong normal form
  | AtaWEq a -- Alpha equality, is weak normal form
  | AtaHEq a -- Alpha equality, is head normal form
  | BtaSEq a -- Single beta reduction, reduces to strong normal form
  | BtaWEq a -- Single beta reduction, reduces to weak normal form
  | BtaHEq a -- Single beta reduction, reduces to head normal form
  | ABtaEq a -- Single applicative beta reduction
  | UnABta a
  | NBtaEq a -- Single normal beta reduction
  | UnNBta a
  | DtaSEq a -- Definition equality; is strong normal form
  | DtaWEq a -- Definition equality; is weak normal form
  | DtaHEq a -- Definition equality; is head normal form
  | EtaSEq a -- Single eta reduction, reduces to strong normal form
  | EtaWEq a -- Single eta reduction, reduces to weak normal form
  | EtaHEq a -- Single eta reduction, reduces to head normal form
  | DefnEq a -- Definition equality
  | TrnsEq a -- Multiple reductions: beta, alpha and/or definitions
  | UnTrEq a
  | TnsWEq a -- Multiple reductions: beta, alpha and/or definitions, reduces to weak normal form
  | TnsHEq a -- Multiple reductions: beta, alpha and/or definitions, reduces to head normal form
  | ATrsEq a -- Multiple reductions: applicative beta, alpha and/or definitions
  | UnATEq a
  | NTrsEq a -- Multiple reductions: normal beta, alpha and/or definitions
  | UnNTEq a
  | NormEq a -- Multiple reductions and/or definitions: normal beta, reduces to strong normal form
  deriving (Eq, Show)

data Bind a
  = Bind Id a
  deriving (Show, Functor)

data Expr a
  = EVar Id                  a
  | ELam !(Bind a) !(Expr a) a
  | EApp !(Expr a) !(Expr a) a
--  deriving (Show)

instance Show (Expr a) where
  show = pprint

instance Eq (Bind a) where
  b1 == b2 = bindId b1 == bindId b2

-- instance Eq (Expr a) where
  -- (EVar x _)      == (EVar y _)      = x == y
  -- (ELam b1 e1 _)  == (ELam b2 e2 _ ) = b1 == b2 && e1  == e2
  -- (EApp e1 e1' _) == (EApp e2 e2' _) = e1 == e2 && e1' == e2'
  -- _               == _               = False

data RExpr
  = RVar Id
  | RLam Id    RExpr
  | RApp RExpr RExpr
  deriving (Eq, Generic)

rExpr :: Expr a -> RExpr
rExpr (EVar x _)    = RVar x
rExpr (ELam b e  _) = RLam (bindId b) (rExpr e )
rExpr (EApp e e' _) = RApp (rExpr  e) (rExpr e')

instance Eq (Expr a) where
  e1 == e2 = rExpr e1 == rExpr e2

instance Hashable RExpr

instance Hashable (Expr a) where
  hashWithSalt i = hashWithSalt i . rExpr

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
  pprint e@(ELam {})    = printf "(\\%s -> %s)" (pprint xs) (pprint body)
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

bindId :: Bind a -> Id
bindId (Bind x _) = x

-------------------------------------------------------------------------------------
-- | Tag Extraction
-------------------------------------------------------------------------------------
class Tagged t where
  tag :: t a -> a

instance Tagged Eqn where
  tag :: Eqn a -> a
  tag (AlphEq x) = x
  tag (AtaSEq x) = x
  tag (AtaWEq x) = x
  tag (AtaHEq x) = x
  tag (BetaEq x) = x
  tag (UnBeta x) = x
  tag (BtaSEq x) = x
  tag (BtaWEq x) = x
  tag (BtaHEq x) = x
  tag (ABtaEq x) = x
  tag (UnABta x) = x
  tag (NBtaEq x) = x
  tag (UnNBta x) = x
  tag (DefnEq x) = x
  tag (DtaSEq x) = x
  tag (DtaWEq x) = x
  tag (DtaHEq x) = x
  tag (EtaaEq x) = x
  tag (UnEtaa x) = x
  tag (EtaSEq x) = x
  tag (EtaWEq x) = x
  tag (EtaHEq x) = x
  tag (TrnsEq x) = x
  tag (UnTrEq x) = x
  tag (TnsWEq x) = x
  tag (TnsHEq x) = x
  tag (ATrsEq x) = x
  tag (UnATEq x) = x
  tag (NTrsEq x) = x
  tag (UnNTEq x) = x
  tag (NormEq x) = x

instance Tagged Bind where
  tag (Bind _   x) = x

instance Tagged Expr where
  tag (EVar _   x) = x
  tag (ELam _ _ x) = x
  tag (EApp _ _ x) = x
