module Simply.TypeChecker where

import Simply.AST (Expression(..))
import qualified Simply.Types as T


type Context = [(String, T.Type)]


typeOfTerm :: String -> Either T.Type String
-- TODO: more of them later?
typeOfTerm "+" = Left $ T.Nat `T.Arr` (T.Nat `T.Arr` T.Nat)
typeOfTerm "-" = Left $ T.Nat `T.Arr` (T.Nat `T.Arr` T.Nat)
typeOfTerm "*" = Left $ T.Nat `T.Arr` (T.Nat `T.Arr` T.Nat)
typeOfTerm "/" = Left $ T.Nat `T.Arr` (T.Nat `T.Arr` T.Nat)
typeOfTerm "%" = Left $ T.Nat `T.Arr` (T.Nat `T.Arr` T.Nat)
typeOfTerm "^" = Left $ T.Nat `T.Arr` (T.Nat `T.Arr` T.Nat)
typeOfTerm "=" = Left $ T.Nat `T.Arr` (T.Nat `T.Arr` T.Boolean)
typeOfTerm ">=" = Left $ T.Nat `T.Arr` (T.Nat `T.Arr` T.Boolean)
typeOfTerm "<=" = Left $ T.Nat `T.Arr` (T.Nat `T.Arr` T.Boolean)
typeOfTerm "&&" = Left $ T.Boolean `T.Arr` (T.Boolean `T.Arr` T.Boolean)
typeOfTerm "||" = Left $ T.Boolean `T.Arr` (T.Boolean `T.Arr` T.Boolean)
typeOfTerm "!" = Left $ T.Boolean `T.Arr` T.Boolean
-- typeOfTerm "T" = Left T.Boolean
-- typeOfTerm "F" = Left T.Boolean
typeOfTerm _ = Right "Term has no type."

addType :: String -> T.Type -> Context -> Context
addType name t ctx = (name, t) : ctx

getType :: String -> Context -> Either T.Type String
getType _ [] = Right "Free Variable has unknown type."
getType name ((varName, t) : ctx)
  | name == varName = Left t
  | otherwise = getType name ctx

typeOf :: Expression -> Either T.Type String
typeOf ast = typeOf' ast []

typeOf' :: Expression -> Context -> Either T.Type String
typeOf' (Natural n) _ = Left T.Nat
typeOf' (Boolean b) _ = Left T.Boolean
typeOf' (Operator op) _ = typeOfTerm op
typeOf' (Macro t) _ = typeOfTerm t
typeOf' (Variable v) env = getType v env
typeOf' (Abstraction arg t body) env =
  let newEnv = addType arg t env
      bodyt  = typeOf' body newEnv in
      case bodyt of
        Left t' -> Left $ T.Arr t t'
        Right e -> Right e
typeOf' (Application left right) env =
  let lt = typeOf' left env
      rt = typeOf' right env in
      case lt of
        Left (T.Arr a b) | Left a == rt -> Left b
        Left (T.Arr a b) -> Right $ "Type mismatch in: " ++ present lt ++ " applied to " ++ present rt ++ "." -- type mismatch
        Right e -> Right e
        -- _ -> Nothing

present :: Either T.Type String -> String
present = either show id