{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Language.Elsa.Eval (elsa, elsaOn) where

import qualified Data.HashMap.Strict  as M
import qualified Data.HashMap.Lazy    as ML
import qualified Data.HashSet         as S
import qualified Data.List            as L
import           Control.Monad.State
import qualified Data.Maybe           as Mb -- (isJust, maybeToList)
import           Language.Elsa.Types
import           Language.Elsa.Utils  (qPushes, qInit, qPop, fromEither)

--------------------------------------------------------------------------------
elsa :: Elsa a -> [Result a]
--------------------------------------------------------------------------------
elsa = elsaOn (const True)

--------------------------------------------------------------------------------
elsaOn :: (Id -> Bool) -> Elsa a -> [Result a]
--------------------------------------------------------------------------------
elsaOn cond p =
  case mkEnv (defns p) of
    Left err -> [err]
    Right g  -> case checkDupEval (evals p) of
      Left err -> [err]
      Right _  -> [result g e | e <- evals p, check e ]
  where
    check = cond . bindId . evName

checkDupEval :: [Eval a] -> CheckM a (S.HashSet Id)
checkDupEval = foldM addEvalId S.empty

addEvalId :: S.HashSet Id -> Eval a -> CheckM a (S.HashSet Id)
addEvalId s e = 
  if S.member (bindId b) s
    then Left  (errDupEval b)
    else Right (S.insert (bindId b) s)
  where
    b = evName e

result :: Env a -> Eval a -> Result a
result g e = fromEither (eval g e)

mkEnv :: [Defn a] -> CheckM a (Env a)
mkEnv = foldM expand M.empty

expand :: Env a -> Defn a -> CheckM a (Env a)
expand g (Defn b e) = 
  if dupId 
    then Left (errDupDefn b)
    else case zs of
      (x,l) : _ -> Left  (Unbound b x l)
      []        -> Right (M.insert (bindId b) e' g)
  where
    dupId           = M.member (bindId b) g
    e'              = subst e g
    zs              = M.toList (freeVars' e')

--------------------------------------------------------------------------------
type CheckM a b = Either (Result a) b
type Env a      = M.HashMap Id (Expr a)
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
isEq (UnBeta _) = isUnBeta
isEq (DefnEq _) = isDefnEq
isEq (TrnsEq _) = isTrnsEq
isEq (UnTrEq _) = isUnTrEq
isEq (NormEq _) = isNormEq


--------------------------------------------------------------------------------
-- | Transitive Reachability
--------------------------------------------------------------------------------
isTrnsEq :: Env a -> Expr a -> Expr a -> Bool
isTrnsEq g e1 e2 = Mb.isJust (findTrans (isEquiv g e2) (canon g e1))

isUnTrEq :: Env a -> Expr a -> Expr a -> Bool
isUnTrEq g e1 e2 = isTrnsEq g e2 e1

findTrans :: (Expr a -> Bool) -> Expr a -> Maybe (Expr a)
findTrans p e = go S.empty (qInit e)
  where
    go seen q = do
      (e, q') <- qPop q
      if S.member e seen
        then go seen q'
        else if p e
             then return e
             else go (S.insert e seen) (qPushes q (betas e))

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
alphaNormal = alphaShift 0

alphaShift :: Int -> Expr a -> Expr a
alphaShift n e = evalState (normalize M.empty e) n

type AlphaM a = State Int a

normalize :: M.HashMap Id Id -> Expr a -> AlphaM (Expr a)
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

rename :: M.HashMap Id Id -> Id -> Id
rename g x = M.lookupDefault x x g

fresh :: AlphaM Id
fresh = do
  n <- get
  put (n + 1)
  return (newAId n)

newAId :: Int -> Id
newAId n = aId ++ show n

_isAId :: Id -> Maybe Int
_isAId x
  | L.isPrefixOf aId x = Just . read . drop 2 $ x
  | otherwise          = Nothing

aId :: String
aId = "$x"

--------------------------------------------------------------------------------
-- | Beta Reduction
--------------------------------------------------------------------------------
isBetaEq :: Env a -> Expr a -> Expr a -> Bool
isBetaEq _ e1 e2 = or [ e1' == e2  | e1' <- betas e1 ]

isUnBeta :: Env a -> Expr a -> Expr a -> Bool
isUnBeta g e1 e2 = isBetaEq g e2 e1

isNormal :: Env a -> Expr a -> Bool
isNormal g = null . betas . (`subst` g)

-- | `betas e` returns the list [e1,...en] of terms obtainable via a single-step
--   beta reduction from `e`.
betas :: Expr a -> [Expr a]
betas (EVar _ _)     = []
betas (ELam b e z)   = [ ELam b e' z | e' <- betas e ]
betas (EApp e1 e2 z) = [ EApp e1' e2 z | e1' <- betas e1 ]
                    ++ [ EApp e1 e2' z | e2' <- betas e2 ]
                    ++ Mb.maybeToList (beta e1 e2)

beta :: Expr a -> Expr a -> Maybe (Expr a)
beta (ELam (Bind x _) e _) e' = substCA e x e'
beta _                    _   = Nothing

substCA :: Expr a -> Id -> Expr a -> Maybe (Expr a)
substCA e x e'           = go [] e
  where
    zs                   = freeVars e'
    bnd  bs zs           = or [ b `isIn` zs | b <- bs ]
    go bs e@(EVar y _)
      | y /= x           = Just e            -- different var, no subst
      | bnd  bs zs       = Nothing           -- same var, but free-var-captured
      | otherwise        = Just e'           -- same var, but no capture
    go bs (EApp e1 e2 l) = do e1' <- go bs e1
                              e2' <- go bs e2
                              Just (EApp e1' e2' l)
    go bs e@(ELam b e1  l)
      | x == bindId b    = Just e            -- subst-var has been rebound
      | otherwise        = do e1' <- go (b:bs) e1
                              Just (ELam b e1' l)

isIn :: Bind a -> S.HashSet Id -> Bool
isIn = S.member . bindId

--------------------------------------------------------------------------------
-- | Evaluation to Normal Form
--------------------------------------------------------------------------------
isNormEq :: Env a -> Expr a -> Expr a -> Bool
isNormEq g e1 e2 = eqVal (subst e2 g) $ evalNbE ML.empty (subst e1 g)
  where
    evalNbE !env e = case e of
      EVar x _            -> Mb.fromMaybe (Neutral x []) $ ML.lookup x env
      ELam (Bind x _) b _ -> Fun $ \val -> evalNbE (ML.insert x val env) b
      EApp f arg _        -> case evalNbE env f of
        Fun f' -> f' (evalNbE env arg)
        Neutral x args -> Neutral x (evalNbE env arg:args)

    eqVal (EVar x _) (Neutral x' [])
      = x == x'
    eqVal (ELam (Bind x _) b _) (Fun f)
      = eqVal b (f (Neutral x []))
    eqVal (EApp f a _) (Neutral x (a':args))
      = eqVal a a' && eqVal f (Neutral x args)
    eqVal _ _ = False

-- | NbE semantic domain
data Value = Fun !(Value -> Value) | Neutral !Id ![Value]

--------------------------------------------------------------------------------
-- | General Helpers
--------------------------------------------------------------------------------
freeVars :: Expr a -> S.HashSet Id
freeVars = S.fromList . M.keys . freeVars'

freeVars' :: Expr a -> M.HashMap Id a
freeVars' (EVar x l)    = M.singleton x l
freeVars' (ELam b e _)  = M.delete (bindId b)    (freeVars' e)
freeVars' (EApp e e' _) = M.union  (freeVars' e) (freeVars' e')

subst :: Expr a -> Env a -> Expr a
subst e@(EVar v _)   su = M.lookupDefault e v su
subst (EApp e1 e2 z) su = EApp (subst e1 su) (subst e2 su) z
subst (ELam b e z)   su = ELam b (subst e su') z
  where
    su'                 = M.delete (bindId b) su

canon :: Env a -> Expr a -> Expr  a
canon g = alphaNormal . (`subst` g)

isEquiv :: Env a -> Expr a -> Expr a -> Bool
isEquiv g e1 e2 = isAlphEq g (subst e1 g) (subst e2 g)
--------------------------------------------------------------------------------
-- | Error Cases
--------------------------------------------------------------------------------

errInvalid :: Bind a -> Expr a -> Eqn a -> Expr a -> Result a
errInvalid b _ eqn _ = Invalid b (tag eqn)

errPartial :: Bind a -> Expr a -> Result a
errPartial b e = Partial b (tag e)

errDupDefn :: Bind a -> Result a
errDupDefn b = DupDefn b (tag b)

errDupEval :: Bind a -> Result a
errDupEval b = DupEval b (tag b)