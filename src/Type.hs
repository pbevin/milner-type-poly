module Type where

import Data.List (union)
import Exp

data Type = BasicType Id | FunType Type Type | TypeVariable Id deriving (Show, Eq)

showType :: Type -> String
showType (BasicType t) = t
showType (FunType a b) = showType a ++ " -> (" ++ showType b ++ ")"
showType (TypeVariable a) = a

typeVariables :: Type -> [Id]
typeVariables (BasicType _) = []
typeVariables (TypeVariable a) = [a]
typeVariables (FunType a b) = typeVariables a `union` typeVariables b
