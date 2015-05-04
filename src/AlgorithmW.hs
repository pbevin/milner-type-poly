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

data InferenceError = IncompatibleTypes Type Type
                    deriving (Show, Eq)

newtype VarContext a = VarContext {
  runV :: ExceptT InferenceError (State Int) a
} deriving (Monad, MonadError InferenceError, MonadState Int, Functor, Applicative)

runTypeChecker :: VarContext a -> a
runTypeChecker a = case runState (runExceptT (runV a)) 0 of
  (Left err, _) -> error (show err)
  (Right result, _) -> result

w :: ([TypedPrefix], Exp) -> (Subst, TypedExp)
w (p, f) = (t, f') where
  (t, f') = runTypeChecker $ w' (p, f)

w' :: ([TypedPrefix], Exp) -> VarContext (Subst, TypedExp)
w' (p, f) = case f of
  Id x -> case findActive x p of
    Just (LambdaPT, _, sigma) -> return (sid, IdT x sigma)
    Just (FixPT, _, sigma) -> return (sid, IdT x sigma)
    Just (LetPT, _, sigma) -> do
      τ <- newVars sigma p
      return (sid, IdT x τ)

  Apply d e -> do
    (r, d') <- w'(p, d)
    (s, e') <- w'(p, e)
    let rho = typeof d'
        sigma = typeof e'

    beta <- newVar
    u <- unify (s <$$> rho) (FunType sigma beta)

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


unify :: Type -> Type -> VarContext Subst
unify t1 t2 = case (t1, t2) of
  (TypeVariable id, _)
    | t1 == t2 -> return sid
    | t1 /= t2 && id `elem` (typeVariables t2) ->
        throwError $ IncompatibleTypes t1 t2
    | otherwise -> return $ snew id t2

  (BasicType _, BasicType _)
    | t1 == t2 -> return sid
    | otherwise ->
        throwError $ IncompatibleTypes t1 t2

  (FunType a b, FunType a' b') -> do
    r <- unify a a'
    s <- unify b b'
    return $ r <> s

  (_, TypeVariable _) -> unify t2 t1

  (_, _) -> throwError $ IncompatibleTypes t1 t2
