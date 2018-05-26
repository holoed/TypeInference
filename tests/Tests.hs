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

          it "apply identity to int" $
            let env = fromList [("id", ForAll (TyLam (TyVar "T1") (TyVar "T1")))] in
            infer env (app (var "id") (lit (I 42))) `shouldBe` TyCon "int" []

          it "type of identity" $
            infer empty (lam "x" (var "x")) `shouldBe` TyLam (TyVar "T1") (TyVar "T1")

          it "type of nested lam that take 2 arg and return first" $
            infer empty (lam "x" (lam "y" (var "x"))) `shouldBe` TyLam (TyVar "T3") (TyLam (TyVar "T2") (TyVar "T3"))
