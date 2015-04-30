module WellTypedSpec where

import Test.Hspec
import TypedPE
import TypedExp
import WellTyped
import Type

int :: Type
int = BasicType "Int"
bool :: Type
bool = BasicType "Bool"

ta :: Type
ta = TypeVariable "a"

aa = FunType ta ta
aint = FunType a int


spec :: Spec
spec = do

  describe "A variable" $ do
    it "is active in the prefix with the same type" $ do
      wellTyped ([LambdaPT "x" int], IdT "x" int) `shouldBe` True
      wellTyped ([LambdaPT "x" bool], IdT "x" int) `shouldBe` False
      wellTyped ([LambdaPT "x" bool, LambdaPT "x" int], IdT "x" int) `shouldBe` True
      wellTyped ([LambdaPT "x" int, LambdaPT "x" bool], IdT "x" int) `shouldBe` False

    -- it "is active in the prefix as a let with a generic instance of the same type" $ do
    --   wellTyped ([LambdaPT "x" ta], IdT "x" int) `shouldBe` True


  describe "A function application" $ do
    it "is true for a well-typed expression" $ do
      let pe = ([LambdaPT "f" $ FunType int bool, LambdaPT "x" int],
                   ApplyT (IdT "f" $ FunType int bool) (IdT "x" int) bool)
      wellTyped pe `shouldBe` True

    it "must not have a badly typed LHS" $ do
      let pe = ([LambdaPT "f" $ FunType bool bool, LambdaPT "x" int],
                   ApplyT (IdT "f" $ FunType int bool) (IdT "x" int) bool)
      wellTyped pe `shouldBe` False

    it "must not have a badly typed RHS" $ do
      let pe = ([LambdaPT "f" $ FunType int bool, LambdaPT "x" int],
                   ApplyT (IdT "f" $ FunType int bool) (IdT "x" bool) bool)
      wellTyped pe `shouldBe` False

    it "must have a function type on the LHS" $ do
      let pe = ([LambdaPT "f" $ int, LambdaPT "x" int],
                   ApplyT (IdT "f" int) (IdT "x" int) bool)
      wellTyped pe `shouldBe` False

    it "must give the right type of argument to the function" $ do
      let pe = ([LambdaPT "f" $ FunType bool bool, LambdaPT "x" int],
                   ApplyT (IdT "f" $ FunType bool bool) (IdT "x" int) bool)
      wellTyped pe `shouldBe` False

    it "must expect the right type of argument from the function" $ do
      let pe = ([LambdaPT "f" $ FunType int int, LambdaPT "x" int],
                   ApplyT (IdT "f" $ FunType int int) (IdT "x" int) bool)
      wellTyped pe `shouldBe` False
