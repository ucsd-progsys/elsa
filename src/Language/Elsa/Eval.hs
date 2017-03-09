{-# LANGUAGE OverloadedStrings #-}

module Language.Elsa.Eval where

-- import           Language.Elsa.UX
-- import           Language.Elsa.Parser
import           Language.Elsa.Types
import           Language.Elsa.Utils (fromEither)
import qualified Data.Map as M

elsa :: Elsa a -> [(Bind a, Result a)]
elsa p = [(evName e, result g e) | e <- evals p]
  where
    g  = mkEnv (defns p)

result :: Env a -> Eval a -> Result a
result g e = fromEither (eval g e)

mkEnv :: [Defn a] -> Env a
mkEnv = error "TBD:mkEnv"

expand :: [Defn a] -> [Defn a]
expand = error "TBD:expand"

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
      | isNormal g e    = return OK
      | otherwise       = Left (errNormal n e)
    go e (s:steps)      = step g n e s >>= (`go` steps)

step :: Env a -> Bind a -> Expr a -> Step a -> CheckM a (Expr a)
step g n e (Step k e')
  | isEq k g e e' = return e'
  | otherwise     = Left (errStep n e k e')

isEq :: Eqn a -> Env a -> Expr a -> Expr a -> Bool
isEq (AlphEq _) = isAlphEq
isEq (BetaEq _) = isBetaEq
isEq (DefnEq _) = isDefnEq

--------------------------------------------------------------------------------
-- | Definition Equivalence
--------------------------------------------------------------------------------

isDefnEq :: Env a -> Expr a -> Expr a -> Bool
isDefnEq = error "TBD:isDefnEq"

--------------------------------------------------------------------------------
-- | Alpha Equivalence
--------------------------------------------------------------------------------

isAlphEq :: Env a -> Expr a -> Expr a -> Bool
isAlphEq = error "TBD:isAlphaEq"

--------------------------------------------------------------------------------
-- | Beta Reduction
--------------------------------------------------------------------------------

isBetaEq :: Env a -> Expr a -> Expr a -> Bool
isBetaEq = error "TBD:isBetaEq"

isNormal :: Env a -> Expr a -> Bool
isNormal = error "TBD:isNormal"

--------------------------------------------------------------------------------
-- | Error Cases
--------------------------------------------------------------------------------
errStep :: Bind a -> Expr a -> Eqn a -> Expr a -> Result a
errStep = error "TBD:errStep"

errNormal :: Bind a -> Expr a -> Result a
errNormal = error "TBD:errNormal"
