module SystemF.TypeChecker where

import Data.Set (Set, toList)

import SystemF.AST (Expression(..))
import SystemF.Evaluator (freeVar, freeTVar)
import qualified SystemF.Types as T


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
addType var type' context = (var, type') : context

removeType :: String -> Context -> Context
removeType _ [] = []
removeType var ((name, type') : ctxs) =
  if var == name
    then removeType var ctxs
    else (name, type') : removeType var ctxs

getType :: String -> Context -> Either T.Type String
getType _ [] = Right "Free Variable has unknown type."
getType name ((var, type') : ctx)
  | name == var = Left type'
  | otherwise = getType name ctx

typeOf :: Expression -> Either T.Type String
typeOf exp = typeOf' exp [] []

-- this should not type-check - FIXED
-- (λ k : A . (Λ A . (λ a : A . a)) k)
-- forall is applied to non-type argument
-- (/ A . (/ A . (\ k : A . k)) ) [Nat] - OK
-- (/ A . (/ A . (\ k : A . k)) [A] ) [Nat] - OK

typeOf' :: Expression -> Context -> Context -> Either T.Type String
typeOf' (Natural n) _ _ = Left T.Nat
typeOf' (Boolean b) _ _ = Left T.Boolean
typeOf' (Operator op) _ _ = typeOfTerm op
typeOf' (Macro t) _ _ = typeOfTerm t
typeOf' (Variable v) termCtx _ = getType v termCtx

typeOf' (Abstraction arg t body) termCtx typeCtx =
  let
    t' = specify t typeCtx
    newTermCtx = addType arg t' termCtx
    bodyT = typeOf' body newTermCtx typeCtx
  in
    case bodyT of
      Left bt -> Left $ T.Arr t' bt
      Right e -> Right e

typeOf' (Application left right) termCtx typeCtx =
  let
    lt = typeOf' left termCtx typeCtx
    rt = typeOf' right termCtx typeCtx
  in
    case rt of
      Right e -> Right e
      Left _ ->
        case lt of
          Left (T.Arr a b) | Left a == rt -> Left b
          Left (T.Arr a b) -> Right $ "APP Type mismatch in: " ++ present lt ++ " applied to " ++ show right ++ ":" ++ present rt ++ "." -- type mismatch
          Right e -> Right e
          _ -> Right $ "APP Type mismatch in: " ++ present lt ++ " applied to " ++ show right ++ ":" ++ present rt ++ "."

typeOf' (TypeApplication (TypeAbstraction par exp) type') termCtx typeCtx =
  let
    t' = specify type' typeCtx -- pokud bude [Nat -> A] Expression pujde misto specify volat typeOf' type' termCtx TypeCtx
    newTypeCtx = addType par t' typeCtx
  in
    typeOf' exp termCtx newTypeCtx -- TYAPP (Urcite forall A . T) (Urcite T') -- pokud T neexistuje - neexistuje typ TYAPP

-- Otazka - Pouzije se vubec nekdy tohle? Jak muze byt TYAPP kdyz jeji left neni TYABS
typeOf' (TypeApplication term type') termCtx typeCtx =
  let termT = typeOf' term termCtx typeCtx
      specified = specify type' typeCtx in
        case termT of
          Left (T.ForAll par t) -> Left $ T.substituteType par specified t
          Left a -> Right $ "TAPP Type mismatch in: " ++ show a ++ " applied to " ++ show specified ++ "." -- type mismatch
          Right e -> Right e

typeOf' (TypeAbstraction par exp) termCtx typeCtx =
  let
    expFV = toList $ freeVar exp
    typFV = freeTVar exp
    freeTermVarAlphaColision = hasTAlphaColision par expFV termCtx
    freeTypeVarAlphaColision = any (standsIn par) $ map (\ name -> specify (T.Parameter name) typeCtx) typFV  -- elem par typFV
    typeAlphaColision = freeTermVarAlphaColision || freeTypeVarAlphaColision
    (par', exp') =
      if typeAlphaColision
        then ((par ++ "'"), replaceFTVar par (par ++ "'") exp) -- pokud je kolize je potreba prejmenovat `par` a prejmenovat `par` i uvnitr `exp`
        else (par, exp)
  in
    case typeOf' exp' termCtx $ removeType par typeCtx of -- TODO: removeType par typeCtx asi ani nema smysl
      Left t -> Left (T.ForAll par' t)
      Right e -> Right e

replaceFTVar :: String -> String -> Expression -> Expression
replaceFTVar old new exp =
  case exp of
    TypeAbstraction par exp' ->
      if old == par
        then exp
        else TypeAbstraction par $ replaceFTVar old new exp'

    Abstraction arg t body -> Abstraction arg (T.substituteType old (T.Parameter new) t) (replaceFTVar old new body)

    Application left right -> Application (replaceFTVar old new left) (replaceFTVar old new right)

    TypeApplication exp t -> TypeApplication (replaceFTVar old new exp) (T.substituteType old (T.Parameter new) t)

    _ -> exp
    -- Variable String
    -- Natural Int
    -- Macro String
    -- Operator String
    -- Boolean Bool

hasTAlphaColision :: String -> [String] -> Context -> Bool
hasTAlphaColision _ [] _ = False
hasTAlphaColision par (fVar : fVars) ctx =
  case getType fVar ctx of
    Left t ->
      standsIn par t
    Right _ ->
      hasTAlphaColision par fVars ctx

standsIn :: String -> T.Type -> Bool
standsIn tVar (T.Parameter str) = tVar == str
standsIn tVar (T.ForAll p t) =
  if tVar == p
    then False
    else standsIn tVar t
standsIn tVar (T.Arr lt rt) =
  (standsIn tVar lt) || (standsIn tVar rt)
standsIn _ _ = False -- Primitive Types only

specify :: T.Type -> Context -> T.Type
specify (T.Parameter name) ctx =
  case getType name ctx of
    Left t -> t
    Right e -> T.Parameter name
specify (T.Arr t1 t2) ctx = T.Arr (specify t1 ctx) (specify t2 ctx)
specify (T.ForAll name type') ctx =
  T.ForAll name $ specify type' ctx
specify type' _ = type'

present :: Either T.Type String -> String
present = either show id