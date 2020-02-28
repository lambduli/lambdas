module Main where

import Text.ParserCombinators.ReadP
import System.IO

import Untyped.Parser (expression)
import Untyped.AST (Expression(..))
import Untyped.Evaluator (normalize, normalStep, normalForm)

main :: IO ()
main = do
  putStrLn "[enter λ expression]"
  line <- prompt ":$ "
  let ast = fst $ last $ readP_to_S expression line
  execCommand ast

execCommand :: Expression -> IO ()
execCommand exp = do
  -- putStr ":$ "
  -- hFlush stdout
  -- print exp
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
    ":print" -> do
      print exp
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

unwrap :: Maybe a -> a
unwrap (Just a) = a


prompt :: String -> IO (String)
prompt msg = do
  putStr msg
  hFlush stdout
  line <- getLine
  return line