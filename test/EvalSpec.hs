module EvalSpec where

import Prelude hiding (pred)
import Test.Hspec
import Data.Map (Map)
import qualified Data.Map as Map
import Exp
import Eval
import GenExp

eval0 t = eval t $ Map.fromList [ ("a", B1 1),
                                  ("b", B3 "hi"),
                                  ("c", B0 True),
                                  ("d", B0 False),
                                  ("e", B2 3.1415927),
                                  ("eq1", F eq1),
                                  ("pred", F pred),
                                  ("times", F times) ]

eq1 :: V -> V
eq1 (B1 1) = B0 True
eq1 _ = B0 False

pred :: V -> V
pred (B1 0) = B1 0
pred (B1 n) = B1 (n-1)

times :: V -> V
times (B1 m) = F timesm
  where timesm (B1 n) = B1 $ m * n


instance Eq V where
  (B0 a) == (B0 b) = a == b
  (B1 a) == (B1 b) = a == b
  (B2 a) == (B2 b) = a == b
  (B3 a) == (B3 b) = a == b
  (F a)  == (F b)  = error "Can't compare functions"
  (W a)  == (W b)  = a == b
  _      == _      = False

spec :: Spec
spec = do
  it "evaluates simple terms" $ do
    eval0 (Id "a") `shouldBe` B1 1
    eval0 (Id "b") `shouldBe` B3 "hi"

  it "evaluates cond" $ do
    eval0 (Cond (Id "c") (Id "b") (Id "e")) `shouldBe` B3 "hi"
    eval0 (Cond (Id "d") (Id "b") (Id "e")) `shouldBe` B2 3.1415927

  describe "Lambda" $ do
    it "returns a wrapped Haskell function" $ do
      let F f = eval0 (Lambda "a" (Id "a"))
      f (B1 17) `shouldBe` B1 17

  describe "Fix" $ do
    it "returns a wrapped Haskell function" $ do
      let F f = eval0 (Fix "f"
                        (Lambda "n"
                          (Cond (Apply (Id "eq1") (Id "n"))
                                (Id "n")
                                (Apply (Apply (Id "times") (Id "n"))
                                       (Apply (Id "f") (Apply (Id "pred") (Id "n")))))))
      f (B1 1) `shouldBe` B1 1
      f (B1 2) `shouldBe` B1 2
      f (B1 3) `shouldBe` B1 6
      f (B1 6) `shouldBe` B1 720
      f (B1 10) `shouldBe` B1 3628800
