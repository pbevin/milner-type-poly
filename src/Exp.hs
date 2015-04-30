module Exp where
import Text.Show.Functions

type Id = String
type Error = String

data Exp = Id String
         | Apply Exp Exp
         | Cond Exp Exp Exp
         | Lambda Id Exp
         | Fix Id Exp
         | Let Id Exp Exp
           deriving (Show, Eq)

data V = B0 {toBool :: Bool}
       | B1 Int
       | B2 Float
       | B3 String
       | F {toFunc :: V->V}
       | W Error
       | Bottom
         deriving Show

isWrong (W _) = True
isWrong _ = False
isFunc (F _) = True
isFunc _ = False
isBool (B0 _) = True
isBool _ = False
