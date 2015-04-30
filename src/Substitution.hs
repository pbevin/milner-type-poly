module Substitution where

import Data.Map (Map)
import qualified Data.Map as Map
import Exp
import Type

type Substitution = Map Id Type
sub :: Substitution -> Id -> Type
sub s a = case Map.lookup a s of
  Just t -> t
  Nothing -> TypeVariable a


involves :: Substitution -> Id -> Bool
s `involves` a = (sub s a /= TypeVariable a) || any (\(b,sb) -> a `occursIn` sb) (Map.toList s)

occursIn :: Id -> Type -> Bool
x `occursIn` (BasicType t) = False
x `occursIn` (TypeVariable a) = a == x
x `occursIn` (FunType a b) = x `occursIn` a || x `occursIn` b
