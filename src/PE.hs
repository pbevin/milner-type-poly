module PE where

import Exp

data Prefix = LambdaP Id | FixP Id | LetP Id deriving (Show, Eq)
type PE = ([Prefix], Exp)

allPrefixedExpressions :: Exp -> [PE]
allPrefixedExpressions = pes []
  where
    pes :: [Prefix] -> Exp -> [PE]
    pes p e = (p,e) : concatMap transitive (pes' p e)
      where transitive (p,e) = pes p e

    pes' p e = case e of
      Id x -> []
      Apply e e' -> [(p,e), (p,e')]
      Cond e e' e'' -> [(p,e), (p,e'), (p,e'')]
      Lambda x e -> [(p ++ [LambdaP x], e)]
      Fix x e -> [(p ++ [FixP x], e)]
      Let x e e' -> [(p, e), (p ++ [LetP x], e')]

-- Bottom of p361
e1 :: Exp
e1 = Lambda "y" $ Let "f" (Lambda "xx" (Apply (Id "x") (Id "y")))
                          (Apply (Id "f") (Id "y"))

