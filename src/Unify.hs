module Unify where

import Control.Monad.Except
import Data.Either
import Data.Monoid
import Exp
import Type
import Subst
import InferenceError

unify :: Type -> Type -> Either InferenceError Subst
unify t1 t2 = unifyWithVars t1 t2 (const True) sid

unifiable :: Type -> Type -> [Id] -> Bool
unifiable t1 t2 genericVars = isRight (unifyWithVars t1 t2 (`elem` genericVars) sid)

unifyWithVars :: Type -> Type -> (Id -> Bool) -> Subst -> Either InferenceError Subst
unifyWithVars t1 t2 allowable s = case (t1, t2) of
  (TypeVariable var, _)
    | t1 == t2 -> return sid
    | t1 /= t2 && var `elem` (typeVariables t2) ->
        throwError $ IncompatibleTypes t1 t2
    | not (allowable var) ->
        throwError $ CannotRedefineBoundTypeVar var t1 t2
    | otherwise -> return $ snew var t2

  (BasicType _, BasicType _)
    | t1 == t2 -> return sid
    | otherwise ->
        throwError $ IncompatibleTypes t1 t2

  (FunType a b, FunType a' b') -> do
    r <- unify a a'
    s <- unify (r |> b) (r |> b')
    return $ r <> s

  (_, TypeVariable _) -> unify t2 t1

  (_, _) -> throwError $ IncompatibleTypes t1 t2
