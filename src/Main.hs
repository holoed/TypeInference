module Main where

import Control.Monad.Trans
import qualified Data.Set as Set
import Data.Map (Map, empty, fromList)
import Fixpoint
import RecursionSchemes
import Monads
import Ast
import Types
import Environment
import Infer (infer)
import System.Console.Haskeline
import Parser (parseExpr)

env :: Env
env = toEnv [("==", TyLam (TyVar "a") (TyLam (TyVar "a") (TyCon "Bool" []))),
             ("+",  TyLam (TyVar "a") (TyLam (TyVar "a") (TyVar "a"))),
             ("-",  TyLam (TyVar "a") (TyLam (TyVar "a") (TyVar "a"))),
             ("*",  TyLam (TyVar "a") (TyLam (TyVar "a") (TyVar "a"))),
             ("/",  TyLam (TyVar "a") (TyLam (TyVar "a") (TyVar "a")))]

process :: String -> IO ()
process input = do
  let ast = parseExpr input
  putStrLn (either id (show . infer env) ast)

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Junior> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> liftIO $ process input
    loop
