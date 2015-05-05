module SubstSpec where

import Data.Monoid
import Test.Hspec
import Test.QuickCheck
import qualified Data.Map as Map
import Exp
import TypedExp
import Subst
import Type

newtype VarName = VarName Id deriving (Show, Eq)
instance Arbitrary VarName where
  arbitrary = do
    name <- elements ["a", "b", "c", "d"]
    return $ VarName name

prop_sid_is_identity :: Type -> Bool
prop_sid_is_identity t = sid |> t == t

prop_sid_is_left_unit :: Subst -> VarName -> Bool
prop_sid_is_left_unit s (VarName t) =
  (sid <> s) |> (TypeVariable t) == s |> (TypeVariable t)

prop_sid_is_right_unit :: Subst -> VarName -> Bool
prop_sid_is_right_unit s (VarName t) =
  (s <> sid) |> (TypeVariable t) == s |> (TypeVariable t)

spec :: Spec
spec = do
  describe "Subst" $ do
    describe "sid" $ do
      it "is an identity map" $ property prop_sid_is_identity
      it "is a left identity under <>" $ property prop_sid_is_left_unit
      it "is a right identity under <>" $ property prop_sid_is_right_unit

    describe "<>" $ do
      it "chains lookups" $ do
        let m1 = snew "a" $ TypeVariable "b"
        let m2 = snew "b" $ TypeVariable "c"
        slookup "a" (m1 <> m2) `shouldBe` TypeVariable "c"
        slookup "b" (m1 <> m2) `shouldBe` TypeVariable "c"
        slookup "c" (m1 <> m2) `shouldBe` TypeVariable "c"
        slookup "d" (m1 <> m2) `shouldBe` TypeVariable "d"

    describe "naturally extended to Type" $ do
      let m1 = snew "a" $ TypeVariable "b"

      it "maps type variables" $ do
        m1 |> TypeVariable "a" `shouldBe` TypeVariable "b"

      it "does not map basic types" $ do
        m1 |> BasicType "Int" `shouldBe` BasicType "Int"

      it "maps inside function types" $ do
        m1 |> FunType (TypeVariable "a") (TypeVariable "a") `shouldBe`
          FunType (TypeVariable "b") (TypeVariable "b")


    describe "naturally extended to typed exps" $ do
      let m1 = snew "a" (BasicType "Int")
            <> snew "b" (BasicType "Bool")

      it "maps a variable" $ do
        m1 |> IdT "x" (TypeVariable "a") `shouldBe`
          IdT "x" (BasicType "Int")

      it "maps an application" $ do
        let f = IdT "f" (FunType (TypeVariable "a") (TypeVariable "b"))
            f'= IdT "f" (FunType (BasicType "Int") (BasicType "Bool"))
            a = IdT "a" (TypeVariable "a")
            a'= IdT "a" (BasicType "Int")
        m1 |> ApplyT f a (TypeVariable "b") `shouldBe`
          ApplyT f' a' (BasicType "Bool")
