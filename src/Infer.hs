module Infer where

import Data.Set (member)
import Data.Map (Map, empty)
import Monads
import Control.Monad (foldM)
import Fixpoint
import RecursionSchemes
import Ast
import Types
import Substitutions

type TypeEnv = Map String TypeScheme
type TypeM = ReaderState (TypeEnv, Type) (Substitutions, Int)

mgu :: Type -> Type -> Substitutions -> TypeM Substitutions
mgu a b subs =
  case (substitute subs a, substitute subs b) of
    (TyVar ta, TyVar tb) | ta == tb -> return subs
    (TyVar ta, _) | not (member ta (getTVarsOfType b)) -> return (extend ta b subs)
    (_, TyVar _) -> mgu b a subs
    (TyLam a1 b1, TyLam a2 b2) -> do subs2 <- mgu b1 b2 subs
                                     mgu a1 a2 subs2
    (TyCon name1 args1, TyCon name2 args2) | name1 == name2 ->
                         foldM (\s (a', b') -> mgu a' b' s) subs (zip args1 args2)
    (x, y) -> fail ("Unable to unify " ++ show x ++ " " ++ show y)

valueToType :: Prim -> Type
valueToType (I _) = TyCon "int" []
valueToType _ = undefined

alg :: ExpF (TypeM Exp) -> TypeM Exp
alg (Lit v) = do (_, bt) <- ask
                 (subs, index) <- get
                 subs' <- mgu (valueToType v) bt subs
                 put (subs', index)
                 return (In (Lit v))
alg _ = undefined

infer :: TypeEnv -> Exp -> Type
infer env e = substitute subs bt
  where ((subs, _), _) = run m ctx state
        m = cataRec alg e
        bt =  TyVar "TBase"
        ctx = (env, bt)
        state = (empty, 0)
