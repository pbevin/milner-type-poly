module UnifySpec where


import Test.Hspec

import Unify
import Control.Monad.Except
import Data.Either
import Data.Monoid
import Exp
import Type
import Subst
import InferenceError

a = TypeVariable "a"
b = TypeVariable "b"
int = BasicType "Int"
bool = BasicType "Bool"

spec :: Spec
spec = do
  it "unifies a variable with a basic type" $ do
    unify (TypeVariable "a") (BasicType "Int") `shouldBe` Right (Subst [("a", BasicType "Int")])

  it "unifies an abstract function with a concrete one" $ do
    unify (FunType a b) (FunType int bool) `shouldBe` Right (Subst [("b", bool), ("a", int)])

  it "fails to unify an over-constrained abstract function" $ do
    unify (FunType a a) (FunType int bool) `shouldBe` Left (IncompatibleTypes int bool)
