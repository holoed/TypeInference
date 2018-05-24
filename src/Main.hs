{-# LANGUAGE DeriveFunctor #-}

module Main where

import Data.Map (Map, empty)
import Fixpoint
import RecursionSchemes
import Monads
import Ast
import Infer (infer)

-- Sample

data ListF a b = Nil | Cons a b deriving Functor

type List a = Fix (ListF a)

alg (Cons x xs) = do xs' <- xs
                     s <- get
                     put (s + 1)
                     return (x * xs')
alg Nil = ask

coAlg (x:xs) = Cons x xs
coAlg [] = Nil

ret :: ReaderState Int Int Int
ret = cataRec alg (anaRec coAlg [1..5])

main :: IO ()
main = print $ infer empty (In (Lit (I 42)))
