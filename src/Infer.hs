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

updateSubs :: (Substitutions -> TypeM Substitutions) -> TypeM ()
updateSubs f =
   do (subs, index) <- get
      subs' <- f subs
      put (subs', index)

getTypeForName :: String -> TypeM Type
getTypeForName n =
  do (env, _) <- ask
     unless (containsSc n env) $ error ("Name " ++ n ++ " not found")
     case findSc n env of
       ForAll t -> return t -- Make fresh
       _        -> fail "Not supported yet"


alg :: ExpF (TypeM Exp) -> TypeM Exp
alg (Lit v) = do (_, bt) <- ask
                 updateSubs $ mgu (valueToType v) bt
                 return (lit v)
alg (Var n) = do (_, bt) <- ask
                 t <- getTypeForName n
                 updateSubs $ mgu t bt
                 return (var n)
alg _ = undefined

infer :: Env -> Exp -> Type
infer env e = substitute subs bt
  where ((subs, _), _) = run m ctx state
        m = cataRec alg e
        bt =  TyVar "TBase"
        ctx = (env, bt)
        state = (empty, 0)
