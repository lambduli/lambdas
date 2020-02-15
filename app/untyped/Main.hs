module Main where

import Text.ParserCombinators.ReadP
import System.IO

import Untyped.Parser (expression)
import Untyped.AST (Expression(..))
import Untyped.Evaluator (normalize, normalStep)

main :: IO ()
main = do
  putStrLn "[enter λ expression]"
  line <- prompt ":$ "
  let ast = fst $ last $ readP_to_S expression line
  execCommand ast

execCommand :: Expression -> IO ()
execCommand exp = do
  putStr ":$ "
  hFlush stdout
  print exp
  cmnd <- prompt "[command or expression]:$ "
  case cmnd of
    ":step" -> execCommand $ normalStep exp
    ":normalize" -> execCommand $ normalize exp
    ":new" -> main
    ":bye" -> return ()
    _ -> execCommand $ fst $ last $ readP_to_S expression cmnd

unwrap :: Maybe a -> a
unwrap (Just a) = a


prompt :: String -> IO (String)
prompt msg = do
  putStr msg
  hFlush stdout
  line <- getLine
  return line