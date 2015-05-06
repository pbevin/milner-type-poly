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

lambdaPT v t = (LambdaPT, v, t)
letPT v t = (LetPT, v, t)

spec :: Spec
spec = do

  describe "A variable" $ do
    it "is active in the prefix with the same type" $ do
      wellTyped ([lambdaPT "x" int], IdT "x" int) `shouldBe` True
      wellTyped ([lambdaPT "x" bool], IdT "x" int) `shouldBe` False
      wellTyped ([lambdaPT "x" bool, lambdaPT "x" int], IdT "x" int) `shouldBe` True
      wellTyped ([lambdaPT "x" int, lambdaPT "x" bool], IdT "x" int) `shouldBe` False

    it "is active in the prefix as a let with a generic instance of the same type" $ do
      wellTyped ([letPT "x" $ TypeVariable "a"], IdT "x" int) `shouldBe` True

    it "must not be a nongeneric instance under a let prefix" $ do
      wellTyped ([lambdaPT "z" $ TypeVariable "a", letPT "x" $ TypeVariable "a"], IdT "x" int)
        `shouldBe` False


  describe "A function application" $ do
    it "can be well-typed" $ do
      let pe = ([lambdaPT "f" $ FunType int bool, lambdaPT "x" int],
                   ApplyT (IdT "f" $ FunType int bool) (IdT "x" int) bool)
      wellTyped pe `shouldBe` True

    it "must not have a badly typed LHS" $ do
      let pe = ([lambdaPT "f" $ FunType bool bool, lambdaPT "x" int],
                   ApplyT (IdT "f" $ FunType int bool) (IdT "x" int) bool)
      wellTyped pe `shouldBe` False

    it "must not have a badly typed RHS" $ do
      let pe = ([lambdaPT "f" $ FunType int bool, lambdaPT "x" int],
                   ApplyT (IdT "f" $ FunType int bool) (IdT "x" bool) bool)
      wellTyped pe `shouldBe` False

    it "must have a function type on the LHS" $ do
      let pe = ([lambdaPT "f" $ int, lambdaPT "x" int],
                   ApplyT (IdT "f" int) (IdT "x" int) bool)
      wellTyped pe `shouldBe` False

    it "must give the right type of argument to the function" $ do
      let pe = ([lambdaPT "f" $ FunType bool bool, lambdaPT "x" int],
                   ApplyT (IdT "f" $ FunType bool bool) (IdT "x" int) bool)
      wellTyped pe `shouldBe` False

    it "must expect the right type of argument from the function" $ do
      let pe = ([lambdaPT "f" $ FunType int int, lambdaPT "x" int],
                   ApplyT (IdT "f" $ FunType int int) (IdT "x" int) bool)
      wellTyped pe `shouldBe` False

  describe "A cond" $ do
    it "can be well-typed" $ do
      let prefix = [lambdaPT "x" bool, lambdaPT "y" int, lambdaPT "z" int]
          exp = CondT (IdT "x" bool) (IdT "y" int) (IdT "z" int) int

      wellTyped (prefix, exp) `shouldBe` True

    it "must not have a badly typed predicate" $ do
      let prefix = [lambdaPT "x" int, lambdaPT "y" int, lambdaPT "z" int]
          exp = CondT (IdT "x" bool) (IdT "y" int) (IdT "z" int) int

      wellTyped (prefix, exp) `shouldBe` False

    it "must not have a badly typed then clause" $ do
      let prefix = [lambdaPT "x" bool, lambdaPT "y" bool, lambdaPT "z" int]
          exp = CondT (IdT "x" bool) (IdT "y" int) (IdT "z" int) int

      wellTyped (prefix, exp) `shouldBe` False

    it "must not have a badly typed else clause" $ do
      let prefix = [lambdaPT "x" bool, lambdaPT "y" int, lambdaPT "z" bool]
          exp = CondT (IdT "x" bool) (IdT "y" int) (IdT "z" int) int

      wellTyped (prefix, exp) `shouldBe` False

    it "must have a boolean condition" $ do
      let prefix = [lambdaPT "x" int, lambdaPT "y" int, lambdaPT "z" int]
          exp = CondT (IdT "x" int) (IdT "y" int) (IdT "z" int) int

      wellTyped (prefix, exp) `shouldBe` False

    it "must have then and else clauses of the same type" $ do
      let prefix = [lambdaPT "x" bool, lambdaPT "y" bool, lambdaPT "z" int]
          exp = CondT (IdT "x" bool) (IdT "y" bool) (IdT "z" int) int

      wellTyped (prefix, exp) `shouldBe` False

    it "must expect the right type" $ do
      let prefix = [lambdaPT "x" bool, lambdaPT "y" int, lambdaPT "z" int]
          exp = CondT (IdT "x" bool) (IdT "y" int) (IdT "z" int) bool

      wellTyped (prefix, exp) `shouldBe` False

--   describe "A lambda" $ do
--     it "can be well-typed" $ do
--       let prefix = []
--           exp = LambdaT "x"
