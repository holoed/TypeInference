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
            infer env (var "x") `shouldBe` TyCon "string" []
            where env = fromList [("x", ForAll (TyCon "string" []) [])]
