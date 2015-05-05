{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Subst where

import Test.QuickCheck
import Data.Monoid
import Exp
import Type
import TypedExp
import TypedPE


newtype Subst = Subst [(Id, Type)] deriving (Show, Eq)

instance Monoid Subst where
  mempty = sid
  mappend (Subst m1) (Subst m2) = Subst (m2 ++ map override m1)
    where override (x, t) = (x, Subst m2 |> t)

slookup :: Id -> Subst -> Type
slookup x (Subst m) = maybe (TypeVariable x) id $ lookup x m

sid :: Subst
sid = Subst []

snew :: Id -> Type -> Subst
snew x t = Subst [(x, t)]

class Typed a where
  (|>) :: Subst -> a -> a

infixl 5 |>

instance Typed Type where
  m |> BasicType t = BasicType t
  m |> TypeVariable a = slookup a m
  m |> FunType a b = FunType (m |> a) (m |> b)

instance Typed TypedExp where
  m |> IdT x t = IdT x (m |> t)
  m |> ApplyT e e' t = ApplyT (m |> e) (m |> e') (m |> t)
  m |> CondT d e e' t = CondT (m |> d) (m |> e) (m |> e') (m |> t)
  m |> LambdaT x e t = LambdaT x (m |> e) (m |> t)
  m |> FixT x e t = LambdaT x (m |> e) (m |> t)
  m |> LetT x d e t = LetT x (m |> d) (m |> e) (m |> t)

instance Typed a => Typed [a] where
  m |> xs = map (m |>) xs

instance Typed TypedPrefix where
  m |> (LambdaPT, x, t) = (LambdaPT, x, m |> t)
  m |> (FixPT,    x, t) = (FixPT,    x, m |> t)
  m |> (LetPT,    x, t) = (LetPT,    x, m |> t)

instance Typed TypedPE where
  m |> (p, e) = (m |> p, m |> e)

instance Arbitrary Subst where
  arbitrary = do
    f <- arbitrary
    return $ Subst f
