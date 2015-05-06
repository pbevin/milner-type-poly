module AlgorithmWSpec where

import Control.Exception (evaluate)
import Test.Hspec
import AlgorithmW
import Exp
import TypedPE
import TypedExp
import Type
import Subst
import InferenceError

bool = BasicType "Bool"
int = BasicType "Int"
a = TypeVariable "a"
b = TypeVariable "b"
c = TypeVariable "c"
t0 = TypeVariable "t0"

spec :: Spec
spec = do
  it "types a lambda-scoped variable access" $ do
    typeCheck ([(LambdaPT, "x", int)], Id "x") `shouldBe`
      Right (sid, IdT "x" int)

  it "types a fix-scoped variable access" $ do
    typeCheck ([(LetPT, "x", int)], Id "x") `shouldBe`
      Right (sid, IdT "x" int)

  it "types a let-scoped variable access" $ do
    typeCheck ([(LetPT, "x", a)], Id "x") `shouldBe`
      Right (sid, IdT "x" t0)

  it "types a function application" $ do
    let prefix = [ (LambdaPT, "f", FunType a b),
                   (LambdaPT, "a", int) ]
        exp = Apply (Id "f") (Id "a")

    let s = Subst [("b", TypeVariable "t0"), ("a", int)]
        texp = ApplyT (IdT "f" $ FunType int t0)
                      (IdT "a" int)
                      (TypeVariable "t0")

    typeCheck (prefix, exp) `shouldBe` Right (s, texp)

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

    typeCheck (prefix, exp) `shouldBe` Right (s, texp)

  it "types a lambda" $ do
    let prefix = []
        exp = Lambda "x" (Id "x")

    let s = Subst []
        texp = LambdaT "x" (IdT "x" t0) (FunType t0 t0)

    typeCheck (prefix, exp) `shouldBe` Right (s, texp)

  it "types a fix" $ do
    let prefix = []
        exp = Fix "x" (Id "x")

    let s = Subst []
        texp = FixT "x" (IdT "x" t0) t0

    typeCheck (prefix, exp) `shouldBe` Right (s, texp)

  it "types a let" $ do
    let prefix = [(LambdaPT, "y", a)]
        exp = Let "x" (Id "y") (Id "x")

    let s = Subst []
        texp = LetT "x" (IdT "y" a) (IdT "x" a) a

    typeCheck (prefix, exp) `shouldBe` Right (s, texp)

  it "refuses to type a malformed function application" $ do
    let prefix = [(LambdaPT, "f", int), (LambdaPT, "a", int)]
        exp = Apply (Id "f") (Id "a")

    typeCheck (prefix, exp) `shouldBe`
      Left (IncompatibleTypes int (FunType int t0))

  it "refuses to type an unknown variable" $ do
    let prefix = []
        exp = Id "x"

    typeCheck ([], Id "x") `shouldBe`
      Left (UnknownVariable "x")

  it "refuses to redefine a type variable under a lambda" $ do
    let prefix = [(LambdaPT, "f", FunType a a), (LambdaPT, "x", int), (LambdaPT, "y", bool)]
        exp = (Cond (Apply (Id "f") (Id "y"))
                    (Apply (Id "f") (Id "x"))
                    (Apply (Id "f") (Id "x")))

    typeCheck (prefix, exp) `shouldBe`
      Left (IncompatibleTypes bool int)

  it "is happy to redefine a type variable under a let" $ do
    let prefix = [(LetPT, "f", FunType a a), (LambdaPT, "x", int), (LambdaPT, "y", bool)]
        exp = Cond (Apply (Id "f") (Id "y"))
                   (Apply (Id "f") (Id "x"))
                   (Apply (Id "f") (Id "x"))

    let s = Subst [("t1", bool), ("t0", bool), ("t3", int), ("t2", int), ("t5", int), ("t4", int)]
        texp = CondT (ApplyT (IdT "f" $ FunType bool bool) (IdT "y" bool) bool)
                     (ApplyT (IdT "f" $ FunType int int) (IdT "x" int) int)
                     (ApplyT (IdT "f" $ FunType int int) (IdT "x" int) int)
                     int


    typeCheck (prefix, exp) `shouldBe` Right (s, texp)
