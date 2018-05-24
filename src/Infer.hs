module Infer where

import Control.Monad
import Data.Map (empty)
import Monads
import RecursionSchemes
import Ast
import Types
import Substitutions
import InferMonad
import Unification

valueToType :: Prim -> Type
valueToType (I _) = TyCon "int" []
valueToType _ = undefined

alg :: ExpF (TypeM Exp) -> TypeM Exp
alg (Lit v) = do (_, bt) <- ask
                 (subs, index) <- get
                 subs' <- mgu (valueToType v) bt subs
                 put (subs', index)
                 return (lit v)
alg (Var n) =  do (env, bt) <- ask
                  (subs, index) <- get
                  unless (containsSc n env) $ error ("Name " ++ n ++ " not found")
                  let (ForAll t _) = findSc n env
                  subs' <- mgu (substitute subs t) bt subs
                  put (subs', index)
                  return (var n)
alg _ = undefined

infer :: Env -> Exp -> Type
infer env e = substitute subs bt
  where ((subs, _), _) = run m ctx state
        m = cataRec alg e
        bt =  TyVar "TBase"
        ctx = (env, bt)
        state = (empty, 0)
