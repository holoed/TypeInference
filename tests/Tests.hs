module Main where

import Data.Bifunctor
import Test.Hspec
import Types
import Environment
import Data.Map (fromList)
import Infer (infer)
import Parser (parseExpr)

env :: Env
env = fromList [("id", ForAll (TyLam (TyVar "a") (TyVar "a")))]

typeOf :: String -> Either String Type
typeOf s = second (infer env) (parseExpr s)

(-->) :: String -> String -> Expectation
(-->) x y = either id show (typeOf x) `shouldBe` y

main :: IO ()
main = hspec $
  describe "Type Inference Tests" $ do

    it "type of a literal" $
      "42" --> "Int"

    it "type of identity" $
      "\\x -> x" --> "(a -> a)"

    it "type of nested lam that take 2 arg and return first" $
      "\\x -> \\y -> x" --> "(a -> (b -> a))"

    it "type of composition" $
      "\\f -> \\g -> \\x -> g (f x)" --> "((a -> b) -> ((b -> c) -> (a -> c)))"

    it "type of fmap" $
      "\\f -> \\m -> \\ctx -> f (m (ctx))" --> "((a -> b) -> ((c -> a) -> (c -> b)))"

    it "type of applying identity to Int" $
      "id 42" --> "Int"

    it "type of conditionals" $ do
      "if True then 5 else 6" --> "Int"
    --  "if True then 5 else False" --> "Unable to unify Bool Int "
    --  "if 5 then True else False" --> "Unable to unify Bool Int"
