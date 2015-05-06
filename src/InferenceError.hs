module InferenceError where

import Exp
import Type

data InferenceError = IncompatibleTypes Type Type
                    | UnknownVariable Id
                    | CannotRedefineBoundTypeVar Id Type Type
                    deriving (Show, Eq)
