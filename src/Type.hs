module Type where

import Test.QuickCheck
import Control.Applicative
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

instance Arbitrary Type where
  arbitrary = sized arbType

arbType :: Int -> Gen Type
arbType 0 = oneof [ BasicType <$> elements ["Bool", "Int", "String", "Float"],
                    TypeVariable <$> elements ["a", "b", "c"] ]
arbType n = do
  k <- choose (0, n-1)
  FunType <$> resize k arbitrary <*> resize (n-1-k) arbitrary
