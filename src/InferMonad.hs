module InferMonad where

import Data.Maybe
import Monads
import Types
import Substitutions
import Environment

type TypeM = ReaderState (Env, Type) (Substitutions, Int)

updateSubs :: (Substitutions -> TypeM Substitutions) -> TypeM ()
updateSubs f =
   do (subs, index) <- get
      subs' <- f subs
      put (subs', index)
