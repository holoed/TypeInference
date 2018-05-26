module Environment where

import Data.Maybe
import Data.Map
import Types
import Prelude hiding (lookup)

type Env = Map String TypeScheme

findSc :: String -> Env -> TypeScheme
findSc n e = fromJust (lookup n e)

containsSc :: String -> Env -> Bool
containsSc = member

addSc :: String -> TypeScheme -> Env -> Env
addSc = insert
