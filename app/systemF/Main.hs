module Main where

import Text.ParserCombinators.ReadP
import System.IO

import SystemF.Parser (expression, typeAnnotation)
import SystemF.AST (Expression(..))
import SystemF.Evaluator (normalize, normalStep)
import SystemF.Types (Type(..))
import SystemF.TypeChecker (typeOf)

main :: IO ()
main = do
  putStrLn "[enter λ2 expression]"
  line <- prompt ":$ "
  let ast = fst $ last $ readP_to_S expression line
  execCommand ast

execCommand :: Expression -> IO ()
execCommand exp = do
  putStr $ unwrapType (typeOf exp) ++ " :: $ "
  hFlush stdout
  print exp
  cmnd <- prompt "[command or expression]:$ "
  case cmnd of
    ":step" -> execCommand $ normalStep exp
    ":normalize" -> execCommand $ normalize exp
    ":new" -> main
    ":bye" -> return ()
    _ -> execCommand $ fst $ last $ readP_to_S expression cmnd

unwrapType :: Either Type String -> String
unwrapType (Left t) = "[" ++ show t ++ "]"
unwrapType (Right e) = "[" ++ e ++ "]"

-- (\ a : forall B . Nat . a [Nat]) (/ A . 23)
-- (/ A . (\ a : A -> A . (\ b : A . a (a b)))) [Nat] ((/ E . (\ i : E . i)) [Nat]) 23
-- (/ A . (\ a : A -> A . (\ b : A . a (a b)))) [Nat] (\ i : Nat . i) 23
-- (/ A . (\ a : A -> A . (\ b : A . a b))) [Nat] (\ i : Nat . i) 23

prompt :: String -> IO (String)
prompt msg = do
  putStr msg
  hFlush stdout
  line <- getLine
  return line