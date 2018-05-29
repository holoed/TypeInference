module Main where

import Control.Monad.Trans
import Data.Map (Map, empty)
import Fixpoint
import RecursionSchemes
import Monads
import Ast
import Infer (infer)
import System.Console.Haskeline
import Parser (parseExpr)

process :: String -> IO ()
process input = do
  let ast = parseExpr input
  putStrLn (either id (show . infer empty) ast)

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Junior> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> liftIO $ process input
    loop
