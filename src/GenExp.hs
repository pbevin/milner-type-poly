module GenExp where
import Control.Applicative
import Test.QuickCheck
import Exp

instance Arbitrary Exp where
  arbitrary = sized arbExp

arbExp :: Int -> Gen Exp
arbExp 0 = Id <$> arbIdent
arbExp n = oneof [ Apply <$> arbFunc <*> arb2,
                   Cond <$> arbFunc <*> arb3 <*> arb3,
                   Lambda <$> arbIdent <*> arbExp (n-1),
                   Fix <$> arbIdent <*> arbExp (n-1),
                   Let <$> arbIdent <*> arb2 <*> arb2 ]
            where arb2 = arbExp (n `div` 2)
                  arb3 = arbExp (n `div` 3)
                  arbFunc = Lambda <$> arbIdent <*> arbExp (n-1)

arbIdent :: Gen Id
arbIdent = elements ["a", "b", "c", "d", "e"]
