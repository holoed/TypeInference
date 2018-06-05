module InferMonad where

import Control.Monad
import Monads
import Types
import Substitutions
import Environment
import Data.Map (fromList)
import Data.Set (Set, toList)

type TypeM = ReaderState (Env, Type) (Substitutions, Int)

newTyVar :: TypeM Type
newTyVar = do (subs, i) <- get
              put (subs, i + 1)
              return (TyVar ("T" ++ show i))

refreshNames :: Set String -> TypeM Substitutions
refreshNames ns = do (TyVar n') <- newTyVar
                     return $ fromList (fmap (\n -> (n, TyVar (n ++ n'))) (toList ns))

getBaseType :: TypeM Type
getBaseType = fmap snd ask

updateSubs :: (Substitutions -> TypeM Substitutions) -> TypeM ()
updateSubs f =
   do (subs, index) <- get
      subs' <- f subs
      put (subs', index)

mkForAll :: Type -> TypeM Type
mkForAll t = fmap (`substitute` t) (refreshNames (getTVarsOfType t))

getTypeForName :: String -> TypeM Type
getTypeForName n =
  do (env, _) <- ask
     unless (containsSc n env) $ throwError ("Name " ++ n ++ " not found.")
     case findSc n env of
       ForAll t -> mkForAll t
       Identity t -> return t

generalise :: Type -> TypeScheme
generalise t@(TyLam _ _) = ForAll t
generalise t = Identity t
