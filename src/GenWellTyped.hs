module GenWellTyped where

import Control.Applicative
import Test.QuickCheck
import Exp
import Type
import TypedExp
import TypedPE
import StdPrefix

bool = BasicType "Bool"

newtype WellTyped = WellTyped { fromWT :: TypedExp } deriving Show

instance Arbitrary WellTyped where
  arbitrary = WellTyped <$> sized (arbWT stdPrefix)

arbWT :: [TypedPrefix] -> Int -> Gen TypedExp
arbWT p 0 = oneof [ arbId p, arbCond p, arbApply p 0 ]
arbWT p n = oneof [ arbLambda p (n-1),
                    arbApply p (n-1),
                    arbLet p (n-1) ]

arbId :: [TypedPrefix] -> Gen TypedExp
arbId p = do
  (_, x, t) <- elements (active p)
  return $ (IdT x t)

arbCond p = do
  (_, x, _) <- elements (findByType bool $ active p)
  (_, y, t) <- elements $ active p
  (_, z, _) <- elements (findByType t $ active p)

  return $ CondT (IdT x bool) (IdT y t) (IdT z t) t

arbApply p 0 = do
  (_, f, FunType a b) <- elements (functions $ active p)
  case findByType a p of
    [] -> return (IdT f $ FunType a b)
    xs -> do
      (_, x, _) <- elements (findByType a $ active p)
      return $ ApplyT (IdT f $ FunType a b) (IdT x a) b

arbApply p n = do
  (j, k) <- splitRange n
  a <- arbConcreteType j
  b <- arbConcreteType k
  f <- arbTypedExp p (FunType a b)
  x <- arbTypedExp p a
  return $ ApplyT f x b

arbLet p 0 = do
  x <- arbIdent
  d <- arbId p
  let t = typeof d
  return $ LetT x t d (IdT x t) t

arbLet p n = do
  (j, k) <- splitRange n
  x <- arbIdent
  a <- arbConcreteType j
  b <- arbConcreteType k
  d <- arbTypedExp p a
  e <- arbTypedExp (pushLet x a p) b
  return $ LetT x a d e b

splitRange n = do
  k <- choose (0, n-1)
  return (k, n-1-k)

arbTypedExp :: [TypedPrefix] -> Type -> Gen TypedExp
arbTypedExp p t = case t of
  FunType a b -> do
    x <- arbIdent
    e <- arbTypedExp (pushLambda x a p) b
    return $ LambdaT x a e t

  _ -> case (findByType t $ active p) of
    [] -> error $ "Can't find type " ++ (show t) ++ "\n\nin: " ++ (show p)
    xs -> do
      (_, x, _) <- elements xs
      return (IdT x t)
  
arbLambda :: [TypedPrefix] -> Int -> Gen TypedExp
arbLambda p 0 = do
  x <- arbIdent
  t <- arbConcreteType 0
  return $ LambdaT x t (IdT x t) (FunType t t)

arbLambda p n = do
  (j, k) <- splitRange n
  x  <- arbIdent
  t' <- arbConcreteType j
  e  <- arbWT (pushLambda x t' p) k
  return $ LambdaT x t' e (FunType t' (typeof e))

arbConcreteType :: Int -> Gen Type
arbConcreteType 0 = BasicType <$> elements ["Int", "Bool"]
arbConcreteType n = do
  (j, k) <- splitRange n
  FunType <$> arbConcreteType j <*> arbConcreteType k


functions :: [TypedPrefix] -> [TypedPrefix]
functions = filter (isFunc . prefixType)
  where isFunc (FunType _ _) = True
        isFunc _ = False

findByType :: Type -> [TypedPrefix] -> [TypedPrefix]
findByType t xs = filter (\p -> prefixType p == t) xs

arbIdent :: Gen Id
arbIdent = elements ["a", "b", "c", "d", "e"]

active :: [TypedPrefix] -> [TypedPrefix]
active xs = active' xs []
  where active' [] _ = []
        active' (p:ps) seen = if (prefixVar p) `elem` seen
                              then active' ps seen
                              else p : active' ps ((prefixVar p):seen)
