module TypedPESpec where

import Test.Hspec
import TypedExp
import TypedPE
import Type

int = BasicType "Int"
--
-- Bottom of p361

a = TypeVariable "a"
b = TypeVariable "b"
c = TypeVariable "c"
x_ab = IdT "x" (FunType a b)
y_a = IdT "y" a
xy_b = ApplyT x_ab y_a b
abb = FunType a (FunType b b)
lamx_xy = LambdaT "x" xy_b abb

acc = FunType a (FunType c c)
f_acc = IdT "f" acc
fy_c = ApplyT f_acc y_a c

typed_e1 = LetT "f" lamx_xy fy_c c

typed_pe1 = ([(LambdaPT, "y", a)], typed_e1)


spec :: Spec
spec = do
  describe "subTypedPEs" $ do
    it "generates the correct list for the example on p361" $ do
      subTypedPEs typed_pe1 `shouldBe`
        [ ([(LambdaPT, "y", a)], typed_e1),
          ([(LambdaPT, "y", a)], lamx_xy),
          ([(LambdaPT, "x", abb), (LambdaPT, "y", a)], xy_b),
          ([(LambdaPT, "x", abb), (LambdaPT, "y", a)], x_ab),
          ([(LambdaPT, "x", abb), (LambdaPT, "y", a)], y_a),
          ([(LetPT, "f", c), (LambdaPT, "y", a)], fy_c),
          ([(LetPT, "f", c), (LambdaPT, "y", a)], f_acc),
          ([(LetPT, "f", c), (LambdaPT, "y", a)], y_a) ]

    describe "genericVars" $ do
      it "includes type vars in a let-bound variable" $ do
        genericVars [(LetPT, "x", TypeVariable "a")] `shouldBe` [ "a" ]

      it "does not include a lambda-bound variable" $ do
        genericVars [(LambdaPT, "x", int)] `shouldBe` []

      it "des not include a let-bound variable shadowed by a lambda" $
        genericVars [ (LambdaPT, "x", TypeVariable "a"),
                      (LetPT, "x", TypeVariable "a") ] `shouldBe` []

  -- Generic variables and isStandard are much easier to understand after
  -- reading Pierce 22.7

  -- describe "genericVariables" $ do
  --   it "works on the example" $ do
  --     genericVariables ([(LambdaPT, "y", a)], lamx_xy) `shouldBe` ["b"]
  --     genericVariables ([(LambdaPT, "y", a)], typed_e1) `shouldBe` ["c"]

  -- describe "isStandard" $ do
  --   it "is true for a standard expression" $ do
  --     let exp = LetT "x" (IdT "y" a) (IdT "x" b) b
  --     isStandard ([], exp) `shouldBe` True

  --   it "is false when e and e' have generic vars in common in a let expression" $ do
  --     let exp = LetT "x" (IdT "y" a) (IdT "x" a) b
  --     isStandard ([], exp) `shouldBe` False
