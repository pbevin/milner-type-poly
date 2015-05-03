module WellTyped where

import Data.List
import Exp
import TypedExp
import TypedPE
import Type

wellTyped (p,e) = case e of
  IdT x t -> activeFunOrLambda x t (reverse p) || activeLet x t (reverse p)
  ApplyT e' e'' t ->
    wellTyped (p, e') && wellTyped (p, e'') && typeof e' == FunType (typeof e'') (typeof e)

activeFunOrLambda :: Id -> Type -> [TypedPrefix] -> Bool
activeFunOrLambda x t [] = False
activeFunOrLambda x t (p:ps) = case p of
  (LambdaPT, x', t') -> if x == x' then t == t' else activeFunOrLambda x t ps
  (FixPT,    x', t') -> if x == x' then t == t' else activeFunOrLambda x t ps
  _ -> activeFunOrLambda x t ps

activeLet x t [] = False
activeLet x t (p:ps) = case p of
  (LetPT, x', s) -> if x == x' then t `isGenericInstanceOf` s else activeLet x t ps
  _ -> activeLet x t ps

isGenericInstanceOf :: Type -> Type -> Bool
isGenericInstanceOf (TypeVariable a) _ = True
isGenericInstanceOf (BasicType a) (BasicType b) = a == b
isGenericInstanceOf (FunType a a') (FunType b b') = isGenericInstanceOf a b && isGenericInstanceOf a' b'
isGenericInstanceOf _ _ = False


-- data TypedExp = IdT String Type
--               | ApplyT TypedExp TypedExp Type
--               | CondT TypedExp TypedExp TypedExp Type
--               | LambdaT Id TypedExp Type
--               | FixT Id TypedExp Type
--               | LetT Id TypedExp TypedExp Type
--                 deriving (Show, Eq)
