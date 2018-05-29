module Infer where

import Control.Arrow (second)
import Data.Map (empty)
import Monads
import RecursionSchemes
import Ast
import Types
import Environment
import Substitutions
import InferMonad
import Unification

valueToType :: Prim -> Type
valueToType (I _) = TyCon "int" []
valueToType _ = undefined

alg :: ExpF (TypeM Exp) -> TypeM Exp
alg (Lit v) = do bt <- getBaseType
                 updateSubs $ mgu (valueToType v) bt
                 return (lit v)

alg (Var n) = do bt <- getBaseType
                 t <- getTypeForName n
                 updateSubs $ mgu t bt
                 return (var n)

alg (App e1 e2) = do t1 <- newTyVar
                     e1' <- local (second (TyLam t1)) e1
                     e2' <- local (\(env, _)  -> (env, t1)) e2
                     return (app e1' e2')

alg (Lam n e) = do bt <- getBaseType
                   t1 <- newTyVar
                   t2 <- newTyVar
                   let t = TyLam t1 t2
                   updateSubs $ mgu t bt
                   e' <- local (\(env, _) -> (addSc n (Identity t1) env, t2)) e
                   return (lam n e')

alg _ = undefined

infer :: Env -> Exp -> Type
infer env e = substitute subs bt
  where (subs, _) = run m ctx state
        m = cataRec alg e
        bt =  TyVar "TBase"
        ctx = (env, bt)
        state = (empty, 0)
