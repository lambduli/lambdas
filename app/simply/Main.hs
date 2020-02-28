module Main where

import Text.ParserCombinators.ReadP
import System.IO

import Simply.Parser (expression)
import Simply.AST (Expression(..))
import Simply.Evaluator (normalize, normalStep, normalForm)
import Simply.Types (Type(..))
import Simply.TypeChecker (typeOf)

main :: IO ()
main = do
  putStrLn "[enter λ-> expression]"
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
    ":type" -> do
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
unwrapType (Left t) = "[" ++ show t ++ "]"
unwrapType (Right e) = "[" ++ e ++ "]"

prompt :: String -> IO (String)
prompt msg = do
  putStr msg
  hFlush stdout
  line <- getLine
  return line