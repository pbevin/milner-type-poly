{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Subst where

import Text.Show.Functions
import Test.QuickCheck
import Data.Monoid
import Exp
import Type
import TypedExp
import TypedPE


newtype Subst = Subst (Id -> Type) deriving Show

instance Monoid Subst where
  mempty = sid
  mappend = scompose

slookup :: Id -> Subst -> Type
slookup x (Subst f) = f x

sid :: Subst
sid = Subst TypeVariable

snew :: Id -> Type -> Subst
snew x t = Subst $ \a -> if x == a then t else TypeVariable a

scompose :: Subst -> Subst -> Subst
scompose (Subst f) (Subst g) = Subst (chain f g) where
  chain :: (Id -> Type) -> (Id -> Type) -> Id -> Type
  chain f g x = case f x of
                  BasicType t -> BasicType t
                  TypeVariable a -> g a
                  FunType a b -> FunType (Subst g |> a) (Subst g |> b)

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
