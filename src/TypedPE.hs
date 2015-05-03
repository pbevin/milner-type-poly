module TypedPE where

import Data.List (union, intersect, (\\))
import Exp
import Type
import TypedExp

data PrefixSpecies = LambdaPT | FixPT | LetPT deriving (Show, Eq)
type TypedPrefix = (PrefixSpecies, Id, Type)
type TypedPE = ([TypedPrefix], TypedExp)

prefixSpecies :: TypedPrefix -> PrefixSpecies
prefixSpecies (p,v,t) = p

prefixVar :: TypedPrefix -> Id
prefixVar (p,v,t) = v

prefixType :: TypedPrefix -> Type
prefixType (p,v,t) = t


subTypedPEs :: TypedPE -> [TypedPE]
subTypedPEs (p,e) = (p,e) : concatMap subTypedPEs (pes' p e)
  where transitive (p,e) = subTypedPEs (p,e)
        pes' :: [TypedPrefix] -> TypedExp -> [TypedPE]
        pes' p e = case e of
          IdT x t -> []
          ApplyT e e' t -> [(p,e), (p,e')]
          CondT e e' e'' t -> [(p,e), (p,e'), (p,e'')]
          LambdaT x e t -> [((LambdaPT, x, t) : p, e)]
          FixT x e t -> [((FixPT, x, t) : p, e)]
          LetT x e e' t -> [(p, e), ((LetPT, x, t) : p, e')]

genericVars :: [TypedPrefix] -> [Id]
genericVars xs = typeVarsIn xs \\ typeVarsIn (filter lambdaBound xs)

lambdaBound :: TypedPrefix -> Bool
lambdaBound p = prefixSpecies p /= LetPT
typeVarsIn :: [TypedPrefix] -> [Id]
typeVarsIn = foldl union [] . map (typeVariables . prefixType)

-- isStandard :: TypedPE -> Bool
-- isStandard pe = all standard (subTypedPEs pe)
--   where
--     standard (p,e) = case e of
--       LetT x e e' t -> null $ genericVariables (p,e) `intersect` genericVariables (p,e')
--       _ -> True



-- genericVariables :: TypedPE -> [Id]
-- genericVariables (p,e) = filter generic (typeVariables $ typeof e)
--   where generic var = var `notElem` funBoundVars p

-- funBoundVars :: [TypedPrefix] -> [Id]
-- funBoundVars [] = []
-- funBoundVars ((p,_,t):ps) = case p of
--   LambdaPT -> funBoundVars ps `union` typeVariables t
--   FixPT    -> funBoundVars ps `union` typeVariables t
--   LetPT    -> funBoundVars ps

-- findSubPE pe e = head $ filter (matchexp e) (subTypedPEs pe)
--   where matchexp e (_,e') = e == e'
