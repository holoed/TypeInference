module Environment where

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Types
import Prelude hiding (lookup)

type Env = Map.Map String TypeScheme

findScheme :: String -> Env -> TypeScheme
findScheme n e = fromJust (Map.lookup n e)

containsScheme :: String -> Env -> Bool
containsScheme = Map.member

addScheme :: String -> TypeScheme -> Env -> Env
addScheme = Map.insert

toScheme :: Type -> TypeScheme
toScheme = ForAll Set.empty

toEnv :: [(String, Type)] -> Env
toEnv xs = Map.map toScheme (Map.fromList xs)
