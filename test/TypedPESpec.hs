module TypedPESpec where

import Test.Hspec
import TypedExp
import TypedPE

spec :: Spec
spec = do
  describe "subTypedPEs" $
    it "generates the correct list for the example on p361" $ do
      subTypedPEs (typed_pe1) `shouldBe`
        [ ([LambdaPT "y" a], typed_e1),
          ([LambdaPT "y" a], lamx_xy),
          ([LambdaPT "y" a, LambdaPT "x" abb], xy_b),
          ([LambdaPT "y" a, LambdaPT "x" abb], x_ab),
          ([LambdaPT "y" a, LambdaPT "x" abb], y_a),
          ([LambdaPT "y" a, LetPT "f" c], fy_c),
          ([LambdaPT "y" a, LetPT "f" c], f_acc),
          ([LambdaPT "y" a, LetPT "f" c], y_a) ]

  -- describe "isStandard" $ do
  --   it "is true for a standard expression" $ do
  --     let exp = LetT "x" (IdT "y" a) (IdT "x" b) b
  --     isStandard exp `shouldBe` True


  describe "genericVariables" $ do
    it "works on the example" $ do
      genericVariables ([LambdaPT "y" a], lamx_xy) `shouldBe` ["b"]
      genericVariables ([LambdaPT "y" a], typed_e1) `shouldBe` ["c"]