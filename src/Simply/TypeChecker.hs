module Simply.TypeChecker where

import qualified Simply.AST as AST
import qualified Simply.Types as Type

type Context = [(String, Type.Type)]

typeOfTerm :: String -> Either Type.Type String
-- TODO: more of them later?
typeOfTerm "+" = Left $ Type.Arr Type.Nat (Type.Arr Type.Nat Type.Nat)
typeOfTerm "T" = Left Type.Boolean
typeOfTerm "F" = Left Type.Boolean
typeOfTerm _ = Right "Term has no type."

addType :: String -> Type.Type -> Context -> Context
addType name t ctx = (name, t) : ctx

getType :: String -> Context -> Either Type.Type String
getType _ [] = Right "Free Variable has unknown type."
getType name ((varName, t) : ctx)
  | name == varName = Left t
  | otherwise = getType name ctx

typeOf :: AST.Expression -> Either Type.Type String
typeOf ast = typeOf' ast []

typeOf' :: AST.Expression -> Context -> Either Type.Type String
typeOf' (AST.Natural n) _ = Left Type.Nat
typeOf' (AST.Boolean b) _ = Left Type.Boolean
typeOf' (AST.Operator op) _ = typeOfTerm op
typeOf' (AST.Macro t) _ = typeOfTerm t
typeOf' (AST.Variable v) env = getType v env
typeOf' (AST.Abstraction arg t body) env =
  let newEnv = addType arg t env
      bodyt  = typeOf' body newEnv in
      case bodyt of
        Left t' -> Left $ Type.Arr t t'
        Right e -> Right e
typeOf' (AST.Application left right) env =
  let lt = typeOf' left env
      rt = typeOf' right env in
      case lt of
        Left (Type.Arr a b) | Left a == rt -> Left b
        Left (Type.Arr a b) -> Right $ "Type mismatch in: " ++ show a ++ " applied to" ++ show b ++ "." -- type mismatch
        Right e -> Right e
        -- _ -> Nothing