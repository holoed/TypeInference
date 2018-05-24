module InferMonad where

import Data.Maybe
import qualified Data.Map as Map
import Monads
import Types
import Substitutions

type Env = Map.Map String TypeScheme

type TypeM = ReaderState (Env, Type) (Substitutions, Int)

findSc :: String -> Env -> TypeScheme
findSc n e = fromJust (Map.lookup n e)

containsSc :: String -> Env -> Bool
containsSc = Map.member
