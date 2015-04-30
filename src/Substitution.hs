module Substitution where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Exp
import Type

type Substitution = Map Id Type
sub :: Substitution -> Id -> Type
sub s a = fromMaybe (TypeVariable a) (Map.lookup a s)

involves :: Substitution -> Id -> Bool
s `involves` a = (sub s a /= TypeVariable a) || any (\(b,sb) -> a `occursIn` sb) (Map.toList s)

occursIn :: Id -> Type -> Bool
x `occursIn` (BasicType t) = False
x `occursIn` (TypeVariable a) = a == x
x `occursIn` (FunType a b) = x `occursIn` a || x `occursIn` b
