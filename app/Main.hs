module Main where

import Text.ParserCombinators.ReadP
import System.IO

-- import UnTyped (normalize)
-- import UnExpressions (factOfThree)
-- import UnParser

-----------------------------------------------------------

-- import Simply.Parser (expression, typeAnnotation)
-- import Simply.AST (Expression(..))
-- import Simply.Types (Type(..))
-- import Simply.TypeChecker (typeOf)
-- import Simply.Evaluator (normalize)

-- main :: IO ()
-- main = do
--   -- let complex = "a b c (d e f g) h i"
--   -- let complex = "(λ a : Bool -> (Bool -> Bool) . (λ b : Bool -> Bool . (λ c : Bool . a (b c) c )))"
--   -- let complex = "(λ a : (Var -> Nat -> Nat) -> Nat -> Bool -> Var . a)"
--   -- let complex = "(λ a : ((Var -> Nat -> Var) -> (Var -> Bool -> Nat)) -> Var . (λ b : Bool . b))"
--   -- let complex = "((Var -> Nat -> Var) -> (Var -> Bool -> Nat)) -> Var -> Bool"
--   -- let complex = "(((Var -> Nat -> Var) -> (Var -> Bool -> Nat)) -> (Var -> (Bool -> Var)))"

--   -- let complex = "(λ a : Bool -> Nat . (λ b : Bool . a b)) (λ e : Bool . 23) T"
--   -- print "                                     original:"
--   -- print complex
--   -- let ast = fst $ last $ readP_to_S expression complex
--   -- print "                                     parsed:"
--   -- print ast
--   -- print "                                     type:"
--   -- print $ typeOf ast


--   line <- getLine
--   let ast = fst $ last $ readP_to_S expression line
--   -- print ast
--   print ""
--   let t = unwrap $ typeOf ast
--   print $ "expression Type :: " ++ show t
--   let res = normalize ast
--   print $ "result: "
--   print res
--   print $ "result Type :: " ++ (show $ unwrap (typeOf res))

--   main

-----------------------------------------------------------

import SystemF.Parser (expression, typeAnnotation)
import SystemF.AST (Expression(..))
import SystemF.Evaluator (normalize, normalStep)
import SystemF.Types (Type(..))
-- import SystemF.TypeChecker (typeOf)

main :: IO ()
main = do
  -- let complex = "a b c (d e f g) h i"
  -- let complex = "(λ a : Bool -> (Bool -> Bool) . (λ b : Bool -> Bool . (λ c : Bool . a (b c) c )))"
  -- let complex = "(λ a : (Var -> Int -> Int) -> Int -> Bool -> Var . a)"
  -- let complex = "(λ a : ((Var -> Int -> Var) -> (Var -> Bool -> Int)) -> Var . (λ b : Bool . b))"
  -- let complex = "((Var -> Int -> Var) -> (Var -> Bool -> Int)) -> Var -> Bool"
  -- let complex = "(((Var -> Int -> Var) -> (Var -> Bool -> Int)) -> (Var -> (Bool -> Var)))"

  -- let complex = "(λ a : Bool -> Int . (λ b : Bool . a b)) (λ e : Bool . 23) T"
  -- print "                                     original:"
  -- print complex
  -- let ast = fst $ last $ readP_to_S expression complex
  -- print "                                     parsed:"
  -- print ast
  -- print "                                     type:"
  -- print $ typeOf ast

  putStrLn "[enter expression]"
  line <- prompt ":$ "
  let ast = fst $ last $ readP_to_S expression line
  execCommand ast
  -- print ""
  -- let t = unwrap $ typeOf ast
  -- print $ "expression Type :: " ++ show t
  -- let res = normalize ast
  -- print $ "result: "
  -- print res
  -- print $ "result Type :: " ++ (show $ unwrap (typeOf res))


execCommand :: Expression -> IO ()
execCommand exp = do
  putStr ":$ "
  hFlush stdout
  print exp
  cmnd <- prompt "[command]:$ "
  case cmnd of
    "step" -> execCommand $ normalStep exp
    "normalize" -> execCommand $ normalize exp
    "new" -> main
    "bye" -> return ()

unwrap :: Maybe a -> a
unwrap (Just a) = a


prompt :: String -> IO (String)
prompt msg = do
  putStr msg
  hFlush stdout
  line <- getLine
  return line