module SystemF.TypeChecker where

import SystemF.AST (Expression(..))
import qualified SystemF.Types as T


type Context = [(String, T.Type)]


typeOfTerm :: String -> Either T.Type String
-- TODO: more of them later?
typeOfTerm "+" = Left $ T.Arr T.Nat (T.Arr T.Nat T.Nat)
typeOfTerm "T" = Left T.Boolean
typeOfTerm "F" = Left T.Boolean
typeOfTerm _ = Right "Term has no type."

addType :: String -> T.Type -> Context -> Context
addType var type' context = (var, type') : context

getType :: String -> Context -> Either T.Type String
getType _ [] = Right "Free Variable has unknown type."
getType name ((var, type') : ctx)
  | name == var = Left type'
  | otherwise = getType name ctx

typeOf :: Expression -> Either T.Type String
typeOf exp = typeOf' exp []

typeOf' :: Expression -> Context -> Either T.Type String
typeOf' (Natural n) _ = Left T.Nat
typeOf' (Boolean b) _ = Left T.Boolean
typeOf' (Operator op) _ = typeOfTerm op
typeOf' (Macro t) _ = typeOfTerm t
typeOf' (Variable v) ctx = getType v ctx
typeOf' (Abstraction arg t body) ctx =
  let t' = lookUp t ctx
      newEnv = addType arg t' ctx
      bodyt  = typeOf' body newEnv in
      case bodyt of
        Left t'' -> Left $ T.Arr t' t''
        Right e -> Right e
typeOf' (Application left right) ctx =
  let lt = typeOf' left ctx
      rt = typeOf' right ctx in
        case rt of
          Right e -> Right e
          Left _ ->
            case lt of
              Left (T.Arr a b) | Left a == rt -> Left b
              Left (T.Arr a b) -> Right $ "APP Type mismatch in: " ++ present lt ++ " applied to " ++ show right ++ ":" ++ present rt ++ "." -- type mismatch
              Right e -> Right e
              -- _ -> Right $ "Something broke " ++ show lt ++ " " ++ show rt ++ " ."
typeOf' (TypeApplication term type') ctx =
  let termT = typeOf' term ctx
      specified = lookUp type' ctx in
        case termT of
          Left (T.ForAll par t) -> Left $ T.substituteType par specified t
          Left a -> Right $ "TAPP Type mismatch in: " ++ show a ++ " applied to " ++ show specified ++ "." -- type mismatch
          Right e -> Right e
typeOf' (TypeAbstraction par exp) ctx =
  case typeOf' exp ctx of
    Left t -> Left (T.ForAll par t)
    Right e -> Right e

lookUp :: T.Type -> Context -> T.Type
lookUp (T.Parameter name) ctx =
  case getType name ctx of
    Left t -> t
    Right e -> T.Parameter name
lookUp (T.Arr t1 t2) ctx = T.Arr (lookUp t1 ctx) (lookUp t2 ctx)
lookUp (T.ForAll name type') ctx =
  T.ForAll name $ lookUp type' ctx
lookUp type' _ = type'

present :: Either T.Type String -> String
present = either show id