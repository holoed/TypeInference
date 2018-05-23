module Substitutions where

import Data.Map (Map)
import Types

type Substitutions = Map String Type

substitute :: Substitutions -> Type -> Type
substitute s (TyVar a) = undefined
