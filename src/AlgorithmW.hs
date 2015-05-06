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
import InferenceError
import Unify

import Debug.Trace

typeCheck :: ([TypedPrefix], Exp) -> TypeCheck (Subst, TypedExp)
typeCheck (p, f) = case runTypeChecker $ w (p, f) of
  Left err -> Left err
  Right (t, f', _) -> Right (t, t |> f')

type TypeCheck a = Either InferenceError a

newtype VarContext a = VarContext {
  runV :: ExceptT InferenceError (State Int) a
} deriving (Monad, MonadError InferenceError, MonadState Int, Functor, Applicative)

runTypeChecker :: VarContext a -> TypeCheck a
runTypeChecker a = case evalState (runExceptT (runV a)) 0 of
  Left err -> Left err
  Right result -> Right result

w :: ([TypedPrefix], Exp) -> VarContext (Subst, TypedExp, Type)
w (p, f) = case f of
  Id x -> case findActive x p of
    Just (LambdaPT, _, sigma) -> return (sid, IdT x sigma, sigma)
    Just (FixPT, _, sigma) -> return (sid, IdT x sigma, sigma)
    Just (LetPT, _, sigma) -> do
      tau <- newVars sigma p
      return (sid, IdT x tau, tau)
    Nothing -> throwError $ UnknownVariable x


  Apply d e -> do
    (r, dT, rho) <- w (p, d)
    (s, eT, sigma) <- w (p, e)

    beta <- newVar
    u <- runUnify (s |> rho) (FunType sigma beta)

    return (u <> s <> r, u |> ApplyT (s |> dT) eT beta, beta)

  Cond d e e' -> do
    (r, dT, rho) <- w (p, d)

    u0 <- runUnify rho (BasicType "Bool")

    (s, eT, sigma) <- w (u0 <> r |> p, e)
    (s', eT', sigma') <- w (s <> u0 <> r |> p, e')

    u <- runUnify (s' |> sigma) sigma'

    return (u <> s' <> s <> u0 <> r,
            u |> CondT (s' <> s <> u0 |> dT)
                         (s' |> eT)
                         eT'
                         sigma,
            sigma)

  Lambda x d -> do
    beta <- newVar
    (r, dT, rho) <- w (pushLambda x beta p, d)

    return (r, LambdaT x dT (FunType (r |> beta) rho), rho)

  Fix x d -> do
    beta <- newVar
    (r, dT, rho) <- w (pushFix x beta p, d)

    u <- runUnify (r |> beta) rho

    let t = (u <> r) |> beta
    return (u <> r, FixT x (u |> dT) t, t)

  Let x d e -> do
    (r, dT, rho) <- w (p, d)
    (s, eT, sigma) <- w (pushLet x rho (r |> p), e)

    let t = s <> r
        f' = LetT x (s |> dT) eT sigma
    return (t, f', sigma)



newVars :: Type -> [TypedPrefix] -> VarContext Type
newVars t p = do
  map <- mapNewVars (typeVariables t `intersect` genericVars p)
  return $ (Subst map) |> t

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


runUnify :: Type -> Type -> VarContext Subst
runUnify t1 t2 = case unify t1 t2 of
  Left err -> throwError err
  Right subst -> return subst
