module Types where

import Data.Set (Set, empty, union, singleton, member)

-- Type

data Type = TyCon String [Type]
          | TyVar String
          | TyLam Type Type deriving Eq

instance Show Type where
  show (TyCon name []) = name
  show (TyVar name) = name
  show (TyLam t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
  show _ = error "Not yet supported"

-- Type Schemes

data TypeScheme = ForAll Type
                | Identity Type

getTVarsOfType :: Type -> Set String
getTVarsOfType (TyVar n) = singleton n
getTVarsOfType (TyLam t1 t2) = getTVarsOfType t1 `union` getTVarsOfType t2
getTVarsOfType (TyCon _ args) = foldl (\ acc t -> acc `union` getTVarsOfType t) empty args
