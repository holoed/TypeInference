{-# LANGUAGE DeriveFunctor #-}

module Ast where

import Fixpoint

data Prim = I Int | B Bool | S String

data ExpF a = Lit Prim
            | Var String
            | App a a
            | Lam String a
            | Let String a a
            | IfThenElse a a a deriving Functor

type Exp = Fix ExpF

lit :: Prim -> Exp
lit v = In (Lit v)
