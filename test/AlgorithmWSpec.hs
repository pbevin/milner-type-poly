module AlgorithmWSpec where

import Test.Hspec
import AlgorithmW
import Exp
import TypedPE
import TypedExp
import Type
import Subst

bool = BasicType "Bool"
int = BasicType "Int"
a = TypeVariable "a"
b = TypeVariable "b"
c = TypeVariable "c"
t0 = TypeVariable "t0"

spec :: Spec
spec = do
  it "types a lambda-scoped variable access" $ do
    w ([(LambdaPT, "x", int)], Id "x") `shouldBe` (sid, IdT "x" int)

  it "types a fix-scoped variable access" $ do
    w ([(LetPT, "x", int)], Id "x") `shouldBe` (sid, IdT "x" int)

  it "types a let-scoped variable access" $ do
    w ([(LetPT, "x", a)], Id "x") `shouldBe` (sid, IdT "x" t0)

  it "types a function application" $ do
    let prefix = [ (LambdaPT, "f", FunType a b),
                   (LambdaPT, "a", int) ]
        exp = Apply (Id "f") (Id "a")

    let s = Subst [("b", TypeVariable "t0"), ("a", int)]
        texp = ApplyT (IdT "f" $ FunType int t0)
                      (IdT "a" int)
                      (TypeVariable "t0")

    w (prefix, exp) `shouldBe` (s, texp)

  it "types a cond" $ do
    let prefix = [ (LambdaPT, "r", a),
                   (LambdaPT, "s", b),
                   (LambdaPT, "t", c) ]
        exp = Cond (Id "r") (Id "s") (Id "t")

    let s = Subst [("a", bool),("b", c)]
        texp = CondT (IdT "r" bool)
                     (IdT "s" c)
                     (IdT "t" c)
                     (TypeVariable "c")

    w (prefix, exp) `shouldBe` (s, texp)
