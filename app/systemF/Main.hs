module Main where

import Text.ParserCombinators.ReadP
import System.IO

import SystemF.Parser (expression)
import SystemF.AST (Expression(..))
import SystemF.Evaluator (normalize, normalStep, normalForm, freeTVar, freeTVar'')
import SystemF.Types (Type(..))
import SystemF.TypeChecker (typeOf)

main :: IO ()
main = do
  putStrLn "Welcome to the λ2 REPL"
  putStrLn "type:"
  putStrLn "  :step       for the next step of the evaluation"
  putStrLn "  :type       for type annotation of the current expression"
  putStrLn "  :isnormal   for checking if the expression is in the normal form"
  putStrLn "  :applyto    for adding an argument to the current expression"
  putStrLn "  :normalize  for the evaluation until the normal form is reached"
  putStrLn "  :reset      for restarting the REPL state"
  putStrLn "  :bye        for exiting the program"

  putStrLn "\n[enter λ2 expression]"
  line <- prompt "λ2 >> "
  case line of
      ":bye" -> return ()
      _ -> do
        let ast = fst $ last $ readP_to_S expression line
        execCommand ast

  -- let ast = fst $ last $ readP_to_S expression line
  -- execCommand ast


execCommand :: Expression -> IO ()
execCommand exp = do
  present'expr exp
  putStrLn "[enter command or λ-> expression]"
  cmnd <- prompt "λ2 >> "
  case cmnd of
    ":step" -> do
      let next = normalStep exp
      print next
      execCommand next
    ":normalize" -> do
      let normal = normalize exp
      execCommand normal
    ":reset" -> main
    ":type" -> do
      putStr ":: "
      putStrLn $ unwrapType (typeOf exp)
      execCommand exp
    ":isnormal" -> do
      putStrLn $ is'normal $ normalForm exp
      execCommand exp
    ":applyto" -> do
      line <- prompt "λ2 >> "
      let ast = fst $ last $ readP_to_S expression line
      execCommand $ Application exp ast
    ":bye" -> return ()
    _ -> execCommand $ fst $ last $ readP_to_S expression cmnd


-- execCommand :: Expression -> IO ()
-- execCommand exp = do
--   cmnd <- prompt "[command or expression]:$ "
--   case cmnd of
--     ":step" -> do
--       let next = normalStep exp
--       putStr ":$ "
--       print next
--       execCommand next
--     ":normalize" -> do
--       let normal = normalize exp
--       putStr ":$ "
--       print normal
--       execCommand normal
--     ":new" -> main
--     ":print" -> do
--       print exp
--       execCommand exp
--     ":type" -> do
--       putStr ":: "
--       putStrLn $ unwrapType (typeOf exp)
--       execCommand exp
--     ":isnormal" -> do
--       print $ normalForm exp
--       execCommand exp
--     ":applyto" -> do
--       putStrLn "[enter λ2 expression]"
--       line <- prompt ":$ "
--       let ast = fst $ last $ readP_to_S expression line
--       execCommand $ Application exp ast
--     ":bye" -> return ()
--     ":freeTVar" -> do
--       print $ freeTVar exp
--       execCommand exp
--     _ -> execCommand $ fst $ last $ readP_to_S expression cmnd


present'expr :: Expression -> IO ()
present'expr expr = do
  putStrLn $ "      " ++ show expr ++ " :: " ++ unwrapType (typeOf expr)


is'normal :: Bool -> String
is'normal False = "      Not in the Normal Form yet."
is'normal True = "      Reached the Normal Form."


unwrapType :: Either Type String -> String
unwrapType (Left t) = show t
unwrapType (Right e) = "~" ++ e ++ "~"

-- (\ a : forall B . Nat . a [Nat]) (/ A . 23)
-- (/ A . (\ a : A -> A . (\ b : A . a (a b)))) [Nat] ((/ E . (\ i : E . i)) [Nat]) 23
-- (/ A . (\ a : A -> A . (\ b : A . a (a b)))) [Nat] (\ i : Nat . i) 23
-- (/ A . (\ a : A -> A . (\ b : A . a b))) [Nat] (\ i : Nat . i) 23

prompt :: String -> IO String
prompt msg = do
  putStr msg
  hFlush stdout
  getLine
