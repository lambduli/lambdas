module Main where

import Text.ParserCombinators.ReadP
import System.IO

import SystemF.Parser (expression)
import SystemF.AST (Expression(..))
import SystemF.Evaluator (normalize, normalStep, normalForm)
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
  cmnd <- prompt "[command or expression]:$ "
  case cmnd of
    ":step" -> do
      let next = normalStep exp
      putStr ":$ "
      print next
      execCommand next
    ":normalize" -> do
      let normal = normalize exp
      putStr ":$ "
      print normal
      execCommand normal
    ":new" -> main
    ":type" -> do
      putStr ":: "
      putStrLn $ unwrapType (typeOf exp)
      execCommand exp
    ":isnormal" -> do
      print $ normalForm exp
      execCommand exp
    ":applyto" -> do
      putStrLn "[enter λ2 expression]"
      line <- prompt ":$ "
      let ast = fst $ last $ readP_to_S expression line
      execCommand $ Application exp ast
    ":bye" -> return ()
    _ -> execCommand $ fst $ last $ readP_to_S expression cmnd

unwrapType :: Either Type String -> String
unwrapType (Left t) = show t
unwrapType (Right e) = "~" ++ e ++ "~"

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