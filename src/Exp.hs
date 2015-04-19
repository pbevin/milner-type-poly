{-# LANGUAGE FlexibleInstances #-}
module Exp where
import Data.Maybe
import Data.Map (Map)
import Data.Function
import Text.Show.Functions
import qualified Data.Map as Map

data Exp = Id String
         | Apply Exp Exp
         | Cond Exp Exp Exp
         | Lambda Id Exp
         | Fix Id Exp
         | Let Id Exp Exp
           deriving (Show, Eq)

data V = B0 {toBool :: Bool}
       | B1 Int
       | B2 Float
       | B3 String
       | F {toFunc :: V->V}
       | W Error
       | Bottom
         deriving Show

type Env = Map Id V

eval :: Exp -> Env -> V
eval (Id x) env = maybe Bottom id $ Map.lookup x env
eval (Apply e1 e2) env =
  let v1 = eval e1 env; v2 = eval e2 env
  in if isFunc v1
     then if isWrong v2 then v2 else (toFunc v1) v2
     else W "Not a function"
eval (Cond e1 e2 e3) env =
  let v1 = eval e1 env; v2 = eval e2 env; v3 = eval e3 env
  in if isBool v1
     then if toBool v1 then v2 else v3
     else W "Not a bool"
eval (Lambda x e) env = F (\v -> eval e (Map.insert x v env))
eval (Fix x e) env = fix (\v -> eval e (Map.insert x v env))
eval (Let x e1 e2) env =
  let v1 = eval e1 env
  in case v1 of
    W _ -> v1
    _   -> eval e2 (Map.insert x v1 env)


data Type = BasicType Id | FunType Type Type | TypeVariable Id deriving (Show, Eq)
showType :: Type -> String
showType (BasicType t) = t
showType (FunType a b) = showType a ++ " -> (" ++ showType b ++ ")"
showType (TypeVariable a) = a



data Prefix = LambdaP Id | FixP Id | LetP Id deriving Show
type PE = ([Prefix], Exp)

pes :: [Prefix] -> Exp -> [PE]
pes p e = [(p,e)] ++ (concatMap transitive $ pes' p e)
  where transitive (p,e) = pes p e

pes' p e = case e of
  Id x -> []
  Apply e e' -> [(p,e), (p,e')]
  Cond e e' e'' -> [(p,e), (p,e'), (p,e'')]
  Lambda x e -> [(p ++ [LambdaP x], e)]
  Fix x e -> [(p ++ [FixP x], e)]
  Let x e e' -> [(p, e), (p ++ [LetP x], e')]

e1 :: Exp
e1 = Lambda "y" $ Let "f" (Lambda "x" (Apply (Id "x") (Id "y")))
                          (Apply (Id "f") (Id "y"))

data TypedExp = IdT Type String
              | ApplyT Type TypedExp TypedExp
              | CondT Type TypedExp TypedExp TypedExp
              | LambdaT Type Id TypedExp
              | FixT Type Id TypedExp
              | LetT Type Id TypedExp TypedExp
                deriving (Show, Eq)




type Id = String
type Error = String

isWrong (W _) = True
isWrong _ = False
isFunc (F _) = True
isFunc _ = False
isBool (B0 _) = True; isBool _ = False


