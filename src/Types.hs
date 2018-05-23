module Types where

-- Type

data Type = TyCon String [Type]
          | TyVar String
          | TyLam Type Type

-- Type Schemes

data TypeScheme = ForAll Type [String]
                | Identity Type
