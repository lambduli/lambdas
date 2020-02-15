module Main where

import Text.ParserCombinators.ReadP
import System.IO

import SystemF.Parser (expression, typeAnnotation)
import SystemF.AST (Expression(..))
import SystemF.Evaluator (normalize, normalStep)
import SystemF.Types (Type(..))
-- import SystemF.TypeChecker (typeOf)

main :: IO ()
main = do
  putStrLn "[enter λ2 expression]"
  line <- prompt ":$ "
  let ast = fst $ last $ readP_to_S expression line
  execCommand ast

execCommand :: Expression -> IO ()
execCommand exp = do
  putStr ":: $ "
  hFlush stdout
  print exp
  cmnd <- prompt "[command]:$ "
  case cmnd of
    ":step" -> execCommand $ normalStep exp
    ":normalize" -> execCommand $ normalize exp
    ":new" -> main
    ":bye" -> return ()

unwrap :: Maybe a -> a
unwrap (Just a) = a


prompt :: String -> IO (String)
prompt msg = do
  putStr msg
  hFlush stdout
  line <- getLine
  return line