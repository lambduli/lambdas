module Main where

import Text.ParserCombinators.ReadP
import System.IO

import Simply.Parser (expression, typeAnnotation)
import Simply.AST (Expression(..))
import Simply.Evaluator (normalize, normalStep)
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
  putStr $ (unwrapType $ typeOf exp) ++ " :: $ "
  hFlush stdout
  print exp
  cmnd <- prompt "[command]:$ "
  case cmnd of
    ":step" -> execCommand $ normalStep exp
    ":normalize" -> execCommand $ normalize exp
    ":new" -> main
    ":bye" -> return ()

unwrapType :: (Show a) => Maybe a -> String
unwrapType (Just a) = "[" ++ show a ++ "]"
unwrapType Nothing = "[type uknown]"

prompt :: String -> IO (String)
prompt msg = do
  putStr msg
  hFlush stdout
  line <- getLine
  return line