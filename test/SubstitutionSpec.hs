module SubstitutionSpec where

import Test.Hspec
import qualified Data.Map as Map
import Substitution
import Type

spec :: Spec
spec = do
  describe "Substitution" $ do
    describe "Example sub1" $ do
      let sub1 = Map.fromList [ ("a", BasicType "Int"),
                                ("b", TypeVariable "a"),
                                ("c", FunType (TypeVariable "d") (BasicType "Bool")) ]

      it "involves a, b, and c" $ do
        sub1 `involves` "a" `shouldBe` True
        sub1 `involves` "b" `shouldBe` True
        sub1 `involves` "c" `shouldBe` True

      it "involves d" $ do
        sub1 `involves` "d" `shouldBe` True

      it "does not involve e" $ do
        sub1 `involves` "e" `shouldBe` False
