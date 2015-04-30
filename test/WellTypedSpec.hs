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
