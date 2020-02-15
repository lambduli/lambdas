module SystemF.TypeChecker where

import qualified SystemF.AST as AST
import qualified SystemF.Types as Type

type Context = [(String, Type.Type)]

typeOfTerm :: String -> Either Type.Type String
-- TODO: more of them later?
typeOfTerm "+" = Left $ Type.Arr Type.Nat (Type.Arr Type.Nat Type.Nat)
typeOfTerm "T" = Left Type.Boolean
typeOfTerm "F" = Left Type.Boolean
typeOfTerm _ = Right "Term has no type."

addType :: String -> Type.Type -> Context -> Context
addType var type' context = (var, type') : context

getType :: String -> Context -> Either Type.Type String
getType _ [] = Right "Free Variable has unknown type."
getType name ((var, type') : ctx)
  | name == var = Left type'
  | otherwise = getType name ctx

typeOf :: AST.Expression -> Either Type.Type String
typeOf exp = typeOf' exp []

typeOf' :: AST.Expression -> Context -> Either Type.Type String
typeOf' (AST.Natural n) _ = Left Type.Nat
typeOf' (AST.Boolean b) _ = Left Type.Boolean
typeOf' (AST.Operator op) _ = typeOfTerm op
typeOf' (AST.Macro t) _ = typeOfTerm t
typeOf' (AST.Variable v) ctx = getType v ctx
typeOf' (AST.Abstraction arg t body) ctx =
  let t' = lookUp t ctx
      newEnv = addType arg t' ctx
      bodyt  = typeOf' body newEnv in
      case bodyt of
        Left t'' -> Left $ Type.Arr t' t''
        Right e -> Right e
typeOf' (AST.Application left right) ctx =
  let lt = typeOf' left ctx
      rt = typeOf' right ctx in
      case lt of
        Left (Type.Arr a b) | Left a == rt -> Left b
        Left (Type.Arr a b) -> Right $ "Type mismatch in: " ++ show a ++ " applied to" ++ show b ++ "." -- type mismatch
        Right e -> Right e
        -- _ -> Right
typeOf' (AST.TypeApplication (AST.TypeAbstraction par exp) type') ctx =
  typeOf' exp $ addType par type' ctx
typeOf' (AST.TypeAbstraction par exp) ctx =
  case typeOf' exp ctx of
    Left t -> Left (Type.ForAll par t)
    Right e -> Right e

lookUp :: Type.Type -> Context -> Type.Type
lookUp (Type.Parameter name) ctx =
  case getType name ctx of
    Left t -> t
    Right e -> Type.Parameter name
lookUp (Type.Arr t1 t2) ctx = Type.Arr (lookUp t1 ctx) (lookUp t2 ctx)
lookUp type' _ = type'