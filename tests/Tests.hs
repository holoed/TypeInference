module Main where

import Test.Hspec
import Types
import Environment
import Data.Map (empty, fromList)
import Ast
import Infer (infer)
import Parser (parseExpr)

parse :: String -> Exp
parse = either (\s -> error ("Unable to type " ++ s)) id . parseExpr

typeOf' :: Env -> String -> Type
typeOf' env = infer env . parse

typeOf :: String -> Type
typeOf = typeOf' empty

main :: IO ()
main = hspec $
  describe "Type Inference Tests" $ do

    it "type of a literal" $
      typeOf "42" `shouldBe` TyCon "int" []

    it "type of a variable in the environment" $
      let env = fromList [("x", ForAll (TyCon "string" []))] in
      typeOf' env "x" `shouldBe` TyCon "string" []

    it "type of applying identity to int" $
      let env = fromList [("id", ForAll (TyLam (TyVar "a") (TyVar "a")))] in
      typeOf' env "id 42" `shouldBe` TyCon "int" []

    it "type of identity" $
      typeOf "\\x -> x" `shouldBe` TyLam (TyVar "a") (TyVar "a")

    it "type of nested lam that take 2 arg and return first" $
      typeOf "\\x -> \\y -> x" `shouldBe` TyLam (TyVar "a") (TyLam (TyVar "b") (TyVar "a"))
