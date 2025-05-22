{-# LANGUAGE OverloadedStrings, BangPatterns, ScopedTypeVariables #-}

module Language.Elsa.Eval (elsa, elsaOn) where

import qualified Data.HashMap.Strict  as M
import qualified Data.HashMap.Lazy    as ML
import qualified Data.HashSet         as S
import qualified Data.List            as L
import           Control.Monad.State
import           Control.Monad        (foldM)
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
isEq (AtaSEq _) = isAlphSEq
isEq (AtaWEq _) = isAlphWEq
isEq (AtaHEq _) = isAlphHEq
isEq (BetaEq _) = isBetaEq
isEq (UnBeta _) = isUnBeta
isEq (BtaSEq _) = isBetaSEq
isEq (BtaWEq _) = isBetaWEq
isEq (BtaHEq _) = isBetaHEq
isEq (ABtaEq _) = isABetaEq
isEq (UnABta _) = isUnABeta
isEq (NBtaEq _) = isNBetaEq
isEq (UnNBta _) = isUnNBeta
isEq (EtaaEq _) = isEtaaEq
isEq (UnEtaa _) = isUnEtaa
isEq (EtaSEq _) = isEtaSEq
isEq (EtaWEq _) = isEtaWEq
isEq (EtaHEq _) = isEtaHEq
isEq (DefnEq _) = isDefnEq
isEq (DtaSEq _) = isDefnSEq
isEq (DtaWEq _) = isDefnWEq
isEq (DtaHEq _) = isDefnHEq
isEq (TrnsEq _) = isTrnsEq
isEq (UnTrEq _) = isUnTrEq
isEq (TnsWEq _) = isTnsWEq
isEq (TnsHEq _) = isTnsHEq
isEq (ATrsEq _) = isATrnsEq
isEq (UnATEq _) = isUnATrEq
isEq (NTrsEq _) = isNTrnsEq
isEq (UnNTEq _) = isUnNTrEq
isEq (NormEq _) = toNormEq


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

-- findTrans with weak normal form check
isTnsWEq :: Env a -> Expr a -> Expr a -> Bool
isTnsWEq = isTnsSEq isWnfEq

-- findTrans with head normal form check
isTnsHEq :: Env a -> Expr a -> Expr a -> Bool
isTnsHEq = isTnsSEq isHnfEq

-- findTrans with selected normal form check
isTnsSEq :: (Env a -> Expr a -> Expr a -> Bool) -> Env a -> Expr a -> Expr a -> Bool
isTnsSEq isNfEq g e1 e2 = maybe False (flip (isNfEq g) e2) (findTrans (isEquiv g e2) (canon g e1))

-- Multiple normal order beta, alpha reductions and/or definitions
isNTrnsEq :: Env a -> Expr a -> Expr a -> Bool
isNTrnsEq = isSTrnsEq norStep

isUnNTrEq :: Env a -> Expr a -> Expr a -> Bool
isUnNTrEq g e1 e2 = isNTrnsEq g e2 e1

-- Multiple applicative order beta, alpha reductions and/or definitions
isATrnsEq :: Env a -> Expr a -> Expr a -> Bool
isATrnsEq = isSTrnsEq appStep

isUnATrEq :: Env a -> Expr a -> Expr a -> Bool
isUnATrEq g e1 e2 = isATrnsEq g e2 e1

-- Multiple beta, alpha reductions and/or definitions, using selected strategy
isSTrnsEq :: forall a. (Expr a -> Maybe (Expr a)) -> Env a -> Expr a -> Expr a -> Bool
isSTrnsEq step g e1 e2 = Mb.isJust (go (isEquiv g e2) (canon g e1))
  where
    go :: (Expr a -> Bool) -> Expr a -> Maybe (Expr a)
    go f e = do
      e' <- step e
      if f e' then
        return e'
      else
        go f e'

--------------------------------------------------------------------------------
-- | Definition Equivalence
--------------------------------------------------------------------------------
isDefnEq :: Env a -> Expr a -> Expr a -> Bool
isDefnEq g e1 e2 = subst e1 g == subst e2 g

-- Defintion Equivalence with strong normal form check
isDefnSEq :: Env a -> Expr a -> Expr a -> Bool
isDefnSEq = isNormEq

-- Defintion Equivalence with weak normal form check
isDefnWEq :: Env a -> Expr a -> Expr a -> Bool
isDefnWEq = isWnfEq

-- Defintion Equivalence with head normal form check
isDefnHEq :: Env a -> Expr a -> Expr a -> Bool
isDefnHEq = isHnfEq

--------------------------------------------------------------------------------
-- | Alpha Equivalence
--------------------------------------------------------------------------------
isAlphEq :: Env a -> Expr a -> Expr a -> Bool
isAlphEq _ e1 e2 = alphaNormal e1 == alphaNormal e2

-- Alpha Equivalence with strong normal form check
isAlphSEq :: Env a -> Expr a -> Expr a -> Bool
isAlphSEq = isAlphPEq isNormEq

-- Alpha Equivalence with weak normal form check
isAlphWEq :: Env a -> Expr a -> Expr a -> Bool
isAlphWEq = isAlphPEq isWnfEq

-- Alpha Equivalence with head normal form check
isAlphHEq :: Env a -> Expr a -> Expr a -> Bool
isAlphHEq = isAlphPEq isHnfEq

-- Alpha Equivalence with provided normal form check
isAlphPEq :: (Env a -> Expr a -> Expr a -> Bool) -> Env a -> Expr a -> Expr a -> Bool
isAlphPEq isNfEq g e1 e2 = (alphaNormal e1 == alphaNormal e2) && isNfEq g e1 e2

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
-- Beta reduction, without any normal form check
isBetaEq :: Env a -> Expr a -> Expr a -> Bool
isBetaEq _ e1 e2 = or [ e1' == e2  | e1' <- betas e1 ]

isUnBeta :: Env a -> Expr a -> Expr a -> Bool
isUnBeta g e1 e2 = isBetaEq g e2 e1

-- Beta reduction, with strong normal form check
isBetaSEq :: Env a -> Expr a -> Expr a -> Bool
isBetaSEq = isBetaPEq isNormEq

-- Beta reduction, with weak normal form check
isBetaWEq :: Env a -> Expr a -> Expr a -> Bool
isBetaWEq = isBetaPEq isWnfEq

-- Beta reduction, with head normal form check
isBetaHEq :: Env a -> Expr a -> Expr a -> Bool
isBetaHEq = isBetaPEq isHnfEq

-- Beta reduction, with provided normal form check
isBetaPEq :: (Env a -> Expr a -> Expr a -> Bool) -> Env a -> Expr a -> Expr a -> Bool
isBetaPEq isNfEq g e1 e2 = or [ isNfEq g e1' e2  | e1' <- betas e1 ]

-- Use normal order evaluation strategy
isNBetaEq :: Env a -> Expr a -> Expr a -> Bool
isNBetaEq _ e1 e2 = norStep e1 == Just e2

isUnNBeta :: Env a -> Expr a -> Expr a -> Bool
isUnNBeta g e1 e2 = isNBetaEq g e2 e1

-- Use applicative order evaluation strategy
isABetaEq :: Env a -> Expr a -> Expr a -> Bool
isABetaEq _ e1 e2 = appStep e1 == Just e2

isUnABeta :: Env a -> Expr a -> Expr a -> Bool
isUnABeta g e1 e2 = isABetaEq g e2 e1

-- norStep is a single normal order reduction
norStep :: Expr a -> Maybe (Expr a)
norStep (EVar {}) = Nothing
norStep (ELam b e l) = do
  e' <- norStep e
  return $ ELam b e' l
norStep (EApp e1@(ELam {}) e2 _) = beta e1 e2
norStep (EApp e1 e2 l) = case norStep e1 of
  Just e1' -> return $ EApp e1' e2 l
  Nothing -> case norStep e2 of
    Just e2' -> return $ EApp e1 e2' l
    Nothing -> Nothing

-- appStep is a single applicative order reduction
appStep :: Expr a -> Maybe (Expr a)
appStep (EVar {}) = Nothing
appStep (ELam b e l) = do
  e' <- appStep e
  return $ ELam b e' l
appStep (EApp e1@(ELam {}) e2 l) = case appStep e1 of
  Just e1' -> Just $ EApp e1' e2 l
  Nothing -> case appStep e2 of
    Just e2' -> Just $ EApp e1 e2' l
    Nothing -> beta e1 e2
appStep (EApp e1 e2 l) = case appStep e1 of
  Just e1' -> return $ EApp e1' e2 l
  Nothing -> case appStep e2 of
    Just e2' -> return $ EApp e1 e2' l
    Nothing -> Nothing

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
-- | Eta Reduction
--------------------------------------------------------------------------------
-- Eta reduction, without any normal form check
isEtaaEq :: Env a -> Expr a -> Expr a -> Bool
isEtaaEq g e1 e2 = go e1 (subst e2 g)
  where
    go e1 e2' = or [e1' == e2' | e1' <- etas g e1]

isUnEtaa :: Env a -> Expr a -> Expr a -> Bool
isUnEtaa g e1 e2 = isEtaaEq g e2 e1

-- Eta reduction, with strong normal form check
isEtaSEq :: Env a -> Expr a -> Expr a -> Bool
isEtaSEq = isEtaPEq isNormEq

-- Eta reduction, with weak normal form check
isEtaWEq :: Env a -> Expr a -> Expr a -> Bool
isEtaWEq = isEtaPEq isWnfEq

-- Eta reduction, with head normal form check
isEtaHEq :: Env a -> Expr a -> Expr a -> Bool
isEtaHEq = isEtaPEq isHnfEq

-- Eta reduction, with provided normal form check
isEtaPEq :: (Env a -> Expr a -> Expr a -> Bool) -> Env a -> Expr a -> Expr a -> Bool
isEtaPEq isNfEq g e1 e2 = or [isNfEq g e1' e2 | e1' <- etas g e1]

-- Search for an eta reduction.
-- Returns the reduced formula if one can be found,
-- returns Nothing if no reductions are possible
eta :: Expr a -> Maybe (Expr a)
eta (ELam x (EApp e (EVar x' _) _) _) =
  let zs = freeVars e in
  if (bindId x == x') && not (isIn x zs)
    then
      Just e
    else Nothing
eta _ = Nothing

etas :: Env a -> Expr a -> [Expr a]
etas g e = go (subst e g)
  where
    go (EVar {})        = []
    -- Pattern where reduction might be possible
    go e'@(ELam b e1 z) = Mb.maybeToList (eta e')
                       ++ [ELam b e1' z | e1' <- go e1]
    go (EApp e1 e2 z)   = [EApp e1' e2 z | e1' <- go e1]
                       ++ [EApp e1 e2' z | e2' <- go e2]

--------------------------------------------------------------------------------
-- | Evaluation to Normal Form
--------------------------------------------------------------------------------
-- Check if e1 is strong normal form
isNormEq :: Env a -> Expr a -> Expr a -> Bool
isNormEq g e1 e2 = (e1' == e2') && nEqVal e2' (nf e2')
  where
    e1' = alphaNormal $ subst e1 g
    e2' = alphaNormal $ subst e2 g
    nf = evalNbE ML.empty

toNormEq :: Env a -> Expr a -> Expr a -> Bool
toNormEq g e1 e2 = nEqVal (subst e2 g) $ evalNbE ML.empty (subst e1 g)

evalNbE :: ML.HashMap Id Value -> Expr a -> Value
evalNbE !env e = case e of
  EVar x _            -> Mb.fromMaybe (Neutral x []) $ ML.lookup x env
  ELam (Bind x _) b _ -> Fun $ \val -> evalNbE (ML.insert x val env) b
  EApp f arg _        -> case evalNbE env f of
    Fun f' -> f' (evalNbE env arg)
    Neutral x args -> Neutral x (evalNbE env arg:args)

nEqVal :: Expr a -> Value -> Bool
nEqVal (EVar x _) (Neutral x' [])
  = x == x'
nEqVal (ELam (Bind x _) b _) (Fun f)
  = nEqVal b (f (Neutral x []))
nEqVal (EApp f a _) (Neutral x (a':args))
  = nEqVal a a' && nEqVal f (Neutral x args)
nEqVal _ _ = False

-- | NbE semantic domain
data Value = Fun !(Value -> Value) | Neutral !Id ![Value]

--------------------------------------------------------------------------------
-- | Evaluation to Weak Normal Form
--------------------------------------------------------------------------------
isWnfEq :: Env a -> Expr a -> Expr a -> Bool
isWnfEq g e1 e2 = (e1' == e2') && (e2' == wnf e2')
  where
    e1' = alphaNormal $ subst e1 g
    e2' = alphaNormal $ subst e2 g
    wnf :: Expr a -> Expr a
    wnf e@(EVar {}) = e
    wnf e@(ELam {}) = e
    wnf (EApp f arg l) = case wnf f of
      f'@ELam {} -> maybe (EApp f' (wnf arg) l) wnf (beta f $ wnf arg)
      f' -> EApp f' (wnf arg) l

--------------------------------------------------------------------------------
-- | Evaluation to Head Normal Form
--------------------------------------------------------------------------------
isHnfEq :: Env a -> Expr a -> Expr a -> Bool
isHnfEq g e1 e2 = (e1' == e2') && (e2' == hnf e2')
  where
    e1' = alphaNormal $ subst e1 g
    e2' = alphaNormal $ subst e2 g
    hnf :: Expr a -> Expr a
    hnf e@(EVar {}) = e
    hnf (ELam bi b a) = ELam bi (hnf b) a
    hnf (EApp f arg l) = case hnf f of
      f'@ELam {} -> maybe (EApp f' (hnf arg) l) hnf (beta f' arg)
      f' -> EApp f' arg l

--------------------------------------------------------------------------------
-- | Evaluation to Weak Head Normal Form
--------------------------------------------------------------------------------
{- isWhnfEq :: Env a -> Expr a -> Expr a -> Bool
isWhnfEq g e1 e2 = (e1' == e2') && (e2' == whnf e2')
  where
    e1' = subst e1 g
    e2' = subst e2 g
    whnf :: Expr a -> Expr a
    whnf e@(EVar {}) = e
    whnf e@(ELam {}) = e
    whnf (EApp f arg l) = case whnf f of
      f'@ELam {} -> maybe (EApp f' arg l) whnf (beta f arg)
      f' -> EApp f' arg l -}

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