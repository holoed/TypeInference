module Infer where

import Data.Map (Map, empty)
import Monads
import Fixpoint
import RecursionSchemes
import Ast
import Types
import Substitutions

type TypeEnv = Map String TypeScheme
type TypeM = ReaderState (TypeEnv, Type) (Substitutions, Int)

mgu :: Type -> Type -> Substitutions -> TypeM Substitutions
mgu t1 t2 subs =
  case (substitute subs t1, substitute subs t2) of
    (TyVar ta, TyVar tb) | ta == tb -> return subs

valueToType :: Prim -> Type
valueToType (I _) = TyCon "int" []

alg :: ExpF (TypeM Exp) -> TypeM Exp
alg (Lit v) = do (_, bt) <- ask
                 (subs, index) <- get
                 subs' <- mgu (valueToType v) bt subs
                 put (subs', index)
                 return (In (Lit v))

infer :: TypeEnv -> Exp -> Type
infer env e = substitute subs bt
  where ((subs, _), _) = run m ctx state
        m = cataRec alg e
        bt =  TyVar "TBase"
        ctx = (env, bt)
        state = (empty, 0)
