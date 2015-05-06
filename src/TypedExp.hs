module TypedExp where

import Type
import Exp

data TypedExp = IdT String Type
              | ApplyT TypedExp TypedExp Type
              | CondT TypedExp TypedExp TypedExp Type
              | LambdaT Id Type TypedExp Type
              | FixT Id Type TypedExp Type
              | LetT Id Type TypedExp TypedExp Type
                deriving (Show, Eq)

typeof :: TypedExp -> Type
typeof (IdT _ t) = t
typeof (ApplyT _ _ t) = t
typeof (CondT _ _ _ t) = t
typeof (LambdaT _ _ _ t) = t
typeof (FixT _ _ _ t) = t
typeof (LetT _ _ _ _ t) = t
