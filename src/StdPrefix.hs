module StdPrefix where

import TypedPE
import Type

stdPrefix :: [TypedPrefix]
stdPrefix = map prefix stdTypes
  where prefix (x, t) = (LetPT, x, t)

stdTypes :: [(String, Type)]
stdTypes = [ ("zero", BasicType "Int"),
             ("one", BasicType "Int"),
             ("two", BasicType "Int"),
             ("succ", FunType (BasicType "Int") (BasicType "Int")),
             ("isZero", FunType (BasicType "Int") (BasicType "Bool")),
             ("eq", FunType (TypeVariable "Int") (FunType (BasicType "Int") (BasicType "Bool"))),
             ("true", BasicType "Bool"),
             ("false", BasicType "Bool"),
             ("plus", FunType (TypeVariable "Int") (FunType (TypeVariable "Int") (TypeVariable "Int"))) ]
