{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AlgorithmW where

import Control.Monad.Except
import Control.Monad.State
import Control.Applicative (Applicative)
import Data.Monoid
import Data.List (intersect)
import Exp
import Type
import TypedExp
import TypedPE
import Subst

import Debug.Trace

newtype VarContext a = VarContext {
  runV :: ExceptT String (State Int) a
} deriving (Monad, MonadError String, MonadState Int, Functor, Applicative)

runTypeChecker :: VarContext a -> a
runTypeChecker a = case runState (runExceptT (runV a)) 0 of
  (Left err, _) -> error err
  (Right result, _) -> result

w :: ([TypedPrefix], Exp) -> (Subst, TypedExp)
w (p, f) = (t, f') where
  (t, f') = runTypeChecker $ w' (p, f)

w' :: ([TypedPrefix], Exp) -> VarContext (Subst, TypedExp)
w' (p, f) = case f of
  Id x -> case findActive x p of
    Just (LambdaPT, _, σ) -> return (sid, IdT x σ)
    Just (FixPT, _, σ) -> return (sid, IdT x σ)
    Just (LetPT, _, σ) -> do
      τ <- newVars σ p
      return (sid, IdT x τ)

  Apply d e -> do
    (r, d') <- w'(p, d)
    (s, e') <- w'(p, e)
    let rho = typeof d'
        sigma = typeof e'

    beta <- newVar
    let TypeVariable t0 = beta
    let u = Subst [("a", BasicType "Int"), (t0, TypeVariable "b")]
    return (u <> s <> r, u <$$> ApplyT (s <$$> d') e' beta)



-- TypedPrefix list is backwards compared to the paper, so
-- innermost scope is to the left. This is because it's simpler
-- to cons a new element onto the left of a list than to concat
-- it onto the right.
findActive :: Id -> [TypedPrefix] -> Maybe TypedPrefix
findActive x [] = Nothing
findActive x (p:ps) = if prefixVar p == x
                      then Just p
                      else findActive x ps

newVars :: Type -> [TypedPrefix] -> VarContext Type
newVars t p = do
  map <- mapNewVars (typeVariables t `intersect` genericVars p)
  return $ (Subst map) <$$> t

mapNewVars :: [Id] -> VarContext [(Id, Type)]
mapNewVars xs = mapM newVarFor xs

newVarFor :: Id -> VarContext (Id, Type)
newVarFor x = do
  v <- newVar
  return (x, v)

newVar :: VarContext Type
newVar = do
  n <- get
  put (n+1)
  return $ TypeVariable ("t" ++ show n)
