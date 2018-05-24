module Main where

import Test.Hspec
import Data.Map (empty)

import Ast
import Types
import Infer

main :: IO ()
main = hspec $ do
        describe "Type Inference Tests" $ do
          it "Infer type of literal" $ do
            infer empty (lit (I 42)) `shouldBe` TyCon "int" []
