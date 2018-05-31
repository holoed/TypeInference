module Main where

import Test.Hspec
import Types
import Environment
import Data.Map (fromList)
import Ast
import Infer (infer)
import Parser (parseExpr)

env :: Env
env = fromList [("id", ForAll (TyLam (TyVar "a") (TyVar "a")))]

parse :: String -> Exp
parse = either (\s -> error ("Unable to type " ++ s)) id . parseExpr

typeOf :: String -> Type
typeOf =  infer env . parse

(-->) :: String -> String -> Expectation
(-->) x y = show (typeOf x) `shouldBe` y

main :: IO ()
main = hspec $
  describe "Type Inference Tests" $ do

    it "type of a literal" $
      "42" --> "int"

    it "type of identity" $
      "\\x -> x" --> "(a -> a)"

    it "type of nested lam that take 2 arg and return first" $
      "\\x -> \\y -> x" --> "(a -> (b -> a))"

    it "type of composition" $
      "\\f -> \\g -> \\x -> g (f x)" --> "((a -> b) -> ((b -> c) -> (a -> c)))"

    it "type of fmap" $
      "\\f -> \\m -> \\ctx -> f (m (ctx))" --> "((a -> b) -> ((c -> a) -> (c -> b)))"

    it "type of applying identity to int" $
      "id 42" --> "int"
