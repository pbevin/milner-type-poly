module WellTyped where

import Data.List
import Exp
import TypedExp
import TypedPE
import Type
import Unify

wellTyped (p,e) = case e of
  IdT x t -> activeFunOrLambda x t p || activeLet x t p

  ApplyT e' e'' t ->
    wellTyped (p, e')
      && wellTyped (p, e'')
      && typeof e' == FunType (typeof e'') (typeof e)

  CondT e e' e'' t ->
    wellTyped (p, e)
      && wellTyped (p, e')
      && wellTyped (p, e'')
      && typeof e == BasicType "Bool"
      && typeof e' == typeof e''
      && typeof e'' == t

  LambdaT x t' e t ->
    wellTyped (pushLambda x t' p, e)
      && t == FunType t' (typeof e)

  FixT x t' e t ->
    wellTyped (pushFix x t' p, e)
      && t == t'
      && t == typeof e

  LetT x t' e e' t ->
    wellTyped (p, e)
      && wellTyped (pushLet x t' p, e')
      && t == typeof e'


activeFunOrLambda :: Id -> Type -> [TypedPrefix] -> Bool
activeFunOrLambda x t [] = False
activeFunOrLambda x t (p:ps) = case p of
  (LambdaPT, x', t') -> if x == x' then t == t' else activeFunOrLambda x t ps
  (FixPT,    x', t') -> if x == x' then t == t' else activeFunOrLambda x t ps
  (LetPT,    x',  _) -> if x == x' then False else activeFunOrLambda x t ps

activeLet :: Id -> Type -> [TypedPrefix] -> Bool
activeLet x t p = case findActive x p of
  Just (LetPT, _, s) -> unifiable s t (genericVars p)
  _ -> False
