module TypedPE where

import Data.List (union)
import Exp
import Type
import TypedExp

data TypedPrefix = LambdaPT Id Type
                 | FixPT    Id Type
                 | LetPT    Id Type
                 deriving (Show, Eq)

type TypedPE = ([TypedPrefix], TypedExp)

-- Bottom of p361

a = TypeVariable "a"
b = TypeVariable "b"
c = TypeVariable "c"
x_ab = IdT "x" (FunType a b)
y_a = IdT "y" a
xy_b = ApplyT x_ab y_a b
abb = FunType a (FunType b b)
lamx_xy = LambdaT "x" xy_b abb

acc = FunType a (FunType c c)
f_acc = IdT "f" acc
fy_c = ApplyT f_acc y_a c

typed_e1 = LetT "f" lamx_xy fy_c c

typed_pe1 = ([LambdaPT "y" a], typed_e1)


subTypedPEs :: TypedPE -> [TypedPE]
subTypedPEs (p,e) = [(p,e)] ++ (concatMap subTypedPEs $ pes' p e)
  where transitive (p,e) = subTypedPEs (p,e)
        pes' :: [TypedPrefix] -> TypedExp -> [TypedPE]
        pes' p e = case e of
          IdT x t -> []
          ApplyT e e' t -> [(p,e), (p,e')]
          CondT e e' e'' t -> [(p,e), (p,e'), (p,e'')]
          LambdaT x e t -> [(p ++ [LambdaPT x t], e)]
          FixT x e t -> [(p ++ [FixPT x t], e)]
          LetT x e e' t -> [(p, e), (p ++ [LetPT x t], e')]

isStandard :: TypedPE -> Bool
isStandard = undefined
-- isStandard :: TypedPE -> Bool
-- isStandard pe = all standard (subTypedPEs pe)
--   where standard (_,


genericVariables :: TypedPE -> [Id]
genericVariables (p,e) = filter generic (typeVariables $ typeof e)
  where generic var = not $ (var `elem` funBoundVars p)

funBoundVars :: [TypedPrefix] -> [Id]
funBoundVars [] = []
funBoundVars (p:ps) = case p of
  LambdaPT _ t -> funBoundVars ps `union` typeVariables t
  FixPT    _ t -> funBoundVars ps `union` typeVariables t
  LetPT    _ t -> funBoundVars ps

findSubPE pe e = head $ filter (matchexp e) (subTypedPEs pe)
  where matchexp e (_,e') = e == e'
