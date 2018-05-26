module Main where

import Data.Map (Map, empty)
import Fixpoint
import RecursionSchemes
import Monads
import Ast
import Infer (infer)

-- Sample
main :: IO ()
main = print $ infer empty (lit (I 42))
