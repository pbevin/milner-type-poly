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
          LambdaT x t' e t -> [(pushLambda x t' p, e)]
          FixT x t' e t -> [(pushFix x t' p, e)]
          LetT x t' e e' t -> [(p, e), (pushLet x t' p, e')]

genericVars :: [TypedPrefix] -> [Id]
genericVars xs = typeVarsIn xs \\ typeVarsIn (filter lambdaBound xs)

lambdaBound :: TypedPrefix -> Bool
lambdaBound p = prefixSpecies p /= LetPT
typeVarsIn :: [TypedPrefix] -> [Id]
typeVarsIn = foldl union [] . map (typeVariables . prefixType)

pushLambda, pushFix, pushLet :: Id -> Type -> [TypedPrefix] -> [TypedPrefix]
pushLambda = pushTP LambdaPT
pushFix    = pushTP FixPT
pushLet    = pushTP LetPT

pushTP :: PrefixSpecies -> Id -> Type -> [TypedPrefix] -> [TypedPrefix]
pushTP s x t p = (s,x,t):p

-- TypedPrefix list is backwards compared to the paper, so
-- innermost scope is to the left. This is because it's simpler
-- to cons a new element onto the left of a list than to concat
-- it onto the right.
findActive :: Id -> [TypedPrefix] -> Maybe TypedPrefix
findActive x [] = Nothing
findActive x (p:ps) = if prefixVar p == x
                      then Just p
                      else findActive x ps
