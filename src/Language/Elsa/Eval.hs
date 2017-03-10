{-# LANGUAGE OverloadedStrings #-}

module Language.Elsa.Eval where

import qualified Data.Map  as M
import qualified Data.Set  as S
import           Control.Monad.State
import           Data.Maybe (maybeToList)
import           Language.Elsa.Types
import           Language.Elsa.Utils (fromEither)

--------------------------------------------------------------------------------
elsa :: Elsa a -> [Result a]
--------------------------------------------------------------------------------
elsa p = case mkEnv (defns p) of
           Left err -> [err]
           Right g  -> [result g e | e <- evals p]

result :: Env a -> Eval a -> Result a
result g e = fromEither (eval g e)

mkEnv :: [Defn a] -> CheckM a (Env a)
mkEnv = foldM expand M.empty

expand :: Env a -> Defn a -> CheckM a (Env a)
expand g (Defn b e) = case M.toList (freeVars' e) of
                        (x, l) : _ -> Left  (Unbound b x l)
                        []         -> Right (M.insert (bindId b) (subst e g) g)

--------------------------------------------------------------------------------
type CheckM a b = Either (Result a) b
type Env a      = M.Map Id (Expr a)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
eval :: Env a -> Eval a -> CheckM a (Result a)
--------------------------------------------------------------------------------
eval g (Eval n e steps) = go e steps
  where
    go e []
      | isNormal g e    = return (OK n)
      | otherwise       = Left (errPartial n e)
    go e (s:steps)      = step g n e s >>= (`go` steps)

step :: Env a -> Bind a -> Expr a -> Step a -> CheckM a (Expr a)
step g n e (Step k e')
  | isEq k g e e' = return e'
  | otherwise     = Left (errInvalid n e k e')

isEq :: Eqn a -> Env a -> Expr a -> Expr a -> Bool
isEq (AlphEq _) = isAlphEq
isEq (BetaEq _) = isBetaEq
isEq (DefnEq _) = isDefnEq

--------------------------------------------------------------------------------
-- | Definition Equivalence
--------------------------------------------------------------------------------
isDefnEq :: Env a -> Expr a -> Expr a -> Bool
isDefnEq g e1 e2 = subst e1 g == subst e2 g

--------------------------------------------------------------------------------
-- | Alpha Equivalence
--------------------------------------------------------------------------------
isAlphEq :: Env a -> Expr a -> Expr a -> Bool
isAlphEq _ e1 e2 = alphaNormal e1 == alphaNormal e2

alphaNormal :: Expr a -> Expr a
alphaNormal e = evalState (normalize M.empty e) 0

type AlphaM a = State Int a

normalize :: M.Map Id Id -> Expr a -> AlphaM (Expr a)
normalize g (EVar x z) =
  return (EVar (rename g x) z)

normalize g (EApp e1 e2 z) = do
  e1' <- normalize g e1
  e2' <- normalize g e2
  return (EApp e1' e2' z)

normalize g (ELam (Bind x z1) e z2) = do
  y     <- fresh
  let g' = M.insert x y g
  e'    <- normalize g' e
  return (ELam (Bind y z1) e' z2)

rename :: M.Map Id Id -> Id -> Id
rename g x = M.findWithDefault x x g

fresh :: AlphaM Id
fresh = do
  n <- get
  put (n + 1)
  return ("$x" ++ show n)

--------------------------------------------------------------------------------
-- | Beta Reduction
--------------------------------------------------------------------------------
isBetaEq :: Env a -> Expr a -> Expr a -> Bool
isBetaEq _ e1 e2 = or [ e1' == e2 | e1' <- betas e1]

isNormal :: Env a -> Expr a -> Bool
isNormal _ = null . betas

-- | `betas e` returns the list [e1,...en] of terms obtainable via a single-step
--   beta reduction from `e`.
betas :: Expr a -> [Expr a]
betas (EVar _ _)     = []
betas (ELam b e z)   = [ ELam b e' z | e' <- betas e ]
betas (EApp e1 e2 z) = [ EApp e1' e2 z | e1' <- betas e1 ]
                    ++ [ EApp e1 e2' z | e2' <- betas e2 ]
                    ++ maybeToList (beta e1 e2)

beta :: Expr a -> Expr a -> Maybe (Expr a)
beta (ELam (Bind x _) e _) e' = substCA e x e'
beta _                    _   = Nothing

substCA :: Expr a -> Id -> Expr a -> Maybe (Expr a)
substCA e x e' = go [] e
  where
    zs                   = freeVars e'
    bnd bs zs            = or [ b `isIn` zs | b <- bs ]
    go bs e@(EVar y _)
      | y /= x           = Just e            -- different var, no subst
      | bnd bs zs        = Nothing           -- same var, but free-var-captured
      | otherwise        = Just e'           -- same var, but no capture
    go bs (EApp e1 e2 l) = do e1' <- go bs e1
                              e2' <- go bs e2
                              Just (EApp e1' e2' l)
    go bs (ELam b e1  l) = do e1' <- go (b:bs) e1
                              Just (ELam b e1' l)

isIn :: Bind a -> S.Set Id -> Bool
isIn = S.member . bindId

--------------------------------------------------------------------------------
-- | Free Variables and Substitution
--------------------------------------------------------------------------------
freeVars :: Expr a -> S.Set Id
freeVars = S.fromList . M.keys . freeVars'

freeVars' :: Expr a -> M.Map Id a
freeVars' (EVar x l)    = M.singleton x l
freeVars' (ELam b e _)  = M.delete (bindId b)    (freeVars' e)
freeVars' (EApp e e' _) = M.union  (freeVars' e) (freeVars' e')

subst :: Expr a -> Env a -> Expr a
subst e@(EVar v _)   su = M.findWithDefault e v su
subst (EApp e1 e2 z) su = EApp (subst e1 su) (subst e2 su) z
subst (ELam b e z)   su = ELam b (subst e su') z
  where
    su'                 = M.delete (bindId b) su

--------------------------------------------------------------------------------
-- | Error Cases
--------------------------------------------------------------------------------
errInvalid :: Bind a -> Expr a -> Eqn a -> Expr a -> Result a
errInvalid b _ eqn _ = Invalid b (tag eqn)

errPartial :: Bind a -> Expr a -> Result a
errPartial b e = Partial b (tag e)
