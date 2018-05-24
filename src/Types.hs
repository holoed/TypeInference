module Types where

import Data.Set (Set, empty, union, singleton, member)

-- Type

data Type = TyCon String [Type]
          | TyVar String
          | TyLam Type Type deriving (Eq, Show)

-- Type Schemes

data TypeScheme = ForAll Type [String]
                | Identity Type

getTVarsOfType :: Type -> Set String
getTVarsOfType (TyVar n) = singleton n
getTVarsOfType (TyLam t1 t2) = getTVarsOfType t1 `union` getTVarsOfType t2
getTVarsOfType (TyCon _ args) = foldl (\ acc t -> acc `union` getTVarsOfType t) empty args
