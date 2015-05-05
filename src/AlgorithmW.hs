{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AlgorithmW where

import Control.Monad.Except
import Control.Monad.State
import Control.Applicative (Applicative)
import Data.Monoid
import Data.Either
import Data.List (intersect)
import Exp
import Type
import TypedExp
import TypedPE
import Subst

import Debug.Trace

type TypeCheck a = Either InferenceError a

data InferenceError = IncompatibleTypes Type Type
                    | UnknownVariable Id
                    deriving (Show, Eq)

newtype VarContext a = VarContext {
  runV :: ExceptT InferenceError (State Int) a
} deriving (Monad, MonadError InferenceError, MonadState Int, Functor, Applicative)

runTypeChecker :: VarContext a -> TypeCheck a
runTypeChecker a = case runState (runExceptT (runV a)) 0 of
  (Left err, _) -> Left err
  (Right result, _) -> Right result

typeCheck :: ([TypedPrefix], Exp) -> TypeCheck (Subst, TypedExp)
typeCheck (p, f) = runTypeChecker $ w (p, f)

w :: ([TypedPrefix], Exp) -> VarContext (Subst, TypedExp)
w (p, f) = case f of
  Id x -> case findActive x p of
    Just (LambdaPT, _, sigma) -> return (sid, IdT x sigma)
    Just (FixPT, _, sigma) -> return (sid, IdT x sigma)
    Just (LetPT, _, sigma) -> do
      τ <- newVars sigma p
      return (sid, IdT x τ)
    Nothing -> throwError $ UnknownVariable x


  Apply d e -> do
    (r, dT) <- w (p, d)
    (s, eT) <- w (p, e)
    let rho = typeof dT
        sigma = typeof eT

    beta <- newVar
    u <- unify (s <$$> rho) (FunType sigma beta)

    return (u <> s <> r, u <$$> ApplyT (s <$$> dT) eT beta)

  Cond d e e' -> do
    (r, dT) <- w (p, d)
    let rho = typeof dT

    u0 <- unify rho (BasicType "Bool")

    (s, eT) <- w ((u0 <> r) <$$> p, e)
    (s', eT') <- w ((s <> u0 <> r) <$$> p, e')
    let sigma = typeof eT
        sigma' = typeof eT'

    u <- unify (s' <$$> sigma) sigma'

    return (u <> s' <> s <> u0 <> r,
            u <$$> CondT ((s' <> s <> u0) <$$> dT)
                         (s' <$$> eT)
                         eT'
                         sigma)

  Lambda x d -> do
    beta <- newVar
    (r, dT) <- w ((LambdaPT, x, beta) : p, d)
    let rho = typeof dT

    return (r, LambdaT x dT (FunType (r <$$> beta) rho))

  Fix x d -> do
    beta <- newVar
    (r, dT) <- w ((FixPT, x, beta) : p, d)
    let rho = typeof dT

    u <- unify (r <$$> beta) rho

    return (u <> r, FixT x (u <$$> dT) ((u <> r) <$$> beta))

  Let x d e -> do
    (r, dT) <- w (p, d)
    let rho = typeof dT
    (s, eT) <- w ((LetPT, x, rho) : (r <$$> p), e)
    let sigma = typeof eT

    let t = s <> r
        f' = LetT x (s <$$> dT) eT sigma
    return (t, f')


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
