module Simply.TypeChecker where

import qualified Simply.AST as AST
import qualified Simply.Types as Type

type Context = [(String, Type.Type)]

typeOfTerm :: String -> Maybe Type.Type
-- TODO: more of them later?
typeOfTerm "+" = Just $ Type.Arr Type.Nat (Type.Arr Type.Nat Type.Nat)
typeOfTerm "T" = Just Type.Boolean
typeOfTerm "F" = Just Type.Boolean
typeOfTerm _ = Nothing

addType :: String -> Type.Type -> Context -> Context
addType name t ctx = (name, t) : ctx

getType :: String -> Context -> Maybe Type.Type
getType _ [] = Nothing
getType name ((varName, t) : ctx)
  | name == varName = Just t
  | otherwise = getType name ctx

typeOf :: AST.Expression -> Maybe Type.Type
typeOf ast = _typeOf ast []

_typeOf :: AST.Expression -> Context -> Maybe Type.Type
_typeOf (AST.Natural n) _ = Just Type.Nat
_typeOf (AST.Boolean b) _ = Just Type.Boolean
_typeOf (AST.Operator op) _ = typeOfTerm op
_typeOf (AST.Macro t) _ = typeOfTerm t
_typeOf (AST.Variable v) env = getType v env
_typeOf (AST.Abstraction arg t body) env =
  let newEnv = addType arg t env
      bodyt  = _typeOf body newEnv in
      case bodyt of
        Just t' -> Just $ Type.Arr t t'
        Nothing -> Nothing
_typeOf (AST.Application left right) env =
  let lt = _typeOf left env
      rt = _typeOf right env in
      case lt of
        Just (Type.Arr a b) | Just a == rt -> Just b
        Just (Type.Arr a _) -> Nothing -- type mismatch
        Nothing -> Nothing
        _ -> Nothing