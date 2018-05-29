module Main where

import Test.Hspec
import Data.Map (empty, fromList)

import Ast
import Types
import Infer

main :: IO ()
main = hspec $
  describe "Type Inference Tests" $ do

    it "type of a literal" $
      infer empty (lit (I 42)) `shouldBe` TyCon "int" []

    it "type of a variable in the environment" $
      let env = fromList [("x", ForAll (TyCon "string" []))] in
      infer env (var "x") `shouldBe` TyCon "string" []

    it "type of applying identity to int" $
      let env = fromList [("id", ForAll (TyLam (TyVar "a") (TyVar "a")))] in
      infer env (app (var "id") (lit (I 42))) `shouldBe` TyCon "int" []

    it "type of identity" $
      infer empty (lam "x" (var "x")) `shouldBe` TyLam (TyVar "a") (TyVar "a")

    it "type of nested lam that take 2 arg and return first" $
      infer empty (lam "x" (lam "y" (var "x"))) `shouldBe` TyLam (TyVar "a") (TyLam (TyVar "b") (TyVar "a"))
