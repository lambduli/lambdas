module SystemF.Evaluator where

import Data.Set (Set)
import qualified Data.Set as Set

import SystemF.AST (Expression(..))
import qualified SystemF.Types as T


normalize :: Expression -> Expression
normalize tree =
  if not (normalForm tree) then
    normalize (normalStep tree)
  else
    tree

normalForm :: Expression -> Bool
normalForm tree =
  case tree of
    TypeAbstraction _ body -> normalForm body

    TypeApplication (TypeAbstraction _ _) _ -> False
    TypeApplication exp _ -> normalForm exp
    
    Abstraction _ _ body -> normalForm body
    
    Application (Abstraction _ _ _) _ -> False
    Application left right -> normalForm left && normalForm right
    
    _ -> True
    -- Variable _ -> True
    -- Number _ -> True
    -- Macro _ -> True
    -- Operator _ -> True
    -- Boolean _ -> True

normalStep :: Expression -> Expression
normalStep tree =
  case tree of
    TypeAbstraction arg exp -> TypeAbstraction arg $ normalStep exp

    TypeApplication (TypeAbstraction arg exp) rightT -> typeBeta arg rightT exp

    Abstraction arg t body -> Abstraction arg t $ normalStep body
    
    Application (Abstraction arg _ body) right -> beta arg right (alpha arg (freeVar right) body)
    
    Application left right ->
      if normalForm left
        then Application left (normalStep right)
        else Application (normalStep left) right
    
    _ -> tree

freeVar :: Expression -> Set String
freeVar = freeVar' Set.empty Set.empty

freeVar' :: Set String -> Set String -> Expression -> Set String
freeVar' bound freeVars tree =
  case tree of
    Variable name ->
      if Set.member name bound then
        freeVars
      else
        Set.insert name freeVars
    Abstraction arg _ body -> freeVar' (Set.insert arg bound) freeVars body
    Application left right ->
      let
        leftFree = freeVar' bound freeVars left
        rightFree = freeVar' bound freeVars right
      in
      Set.union leftFree rightFree
    TypeAbstraction p exp -> freeVar' bound freeVars exp
    TypeApplication left t -> freeVar' bound freeVars left
    _ -> freeVars -- Number, Boolean, Macro, Operator

alpha :: String -> Set String -> Expression -> Expression
alpha arg freeArg tree =
  let
    alphaCurr = alpha arg freeArg
  in
  case tree of
    Abstraction argument t body ->
      if arg == argument then -- shadowing
        tree
      else if Set.member arg (freeVar body) && Set.member argument freeArg then
      -- freeVar original Argument in Body && current Argument in conflict with freeVar Vars from Right Value
        let
          newName = "_" ++ argument ++ "_"
          replacement = Variable ("_" ++ argument ++ "_")
        in
        Abstraction newName t (alphaCurr (beta argument replacement body))
      else tree
    Application left right -> Application (alphaCurr left) (alphaCurr right)
    TypeAbstraction p exp -> TypeAbstraction p $ alphaCurr exp
    TypeApplication left t -> TypeApplication (alphaCurr left) t
    _ -> tree

beta :: String -> Expression -> Expression -> Expression
beta arg value target =
  let
    betaCurr = beta arg value
  in
  case target of
    Variable name ->
      if arg == name
        then value
        else target
    Abstraction argName t body ->
      if arg == argName
        then target
        else Abstraction argName t (betaCurr body)
    Application left right -> Application (betaCurr left) (betaCurr right)
    TypeAbstraction p exp -> TypeAbstraction p $ betaCurr exp
    TypeApplication exp t -> TypeApplication (betaCurr exp) t
    _ -> target
    -- Number n -> Number n
    -- Macro t -> Macro t
    -- Boolean b -> Boolean b
    -- Operator op -> Operator op

typeBeta :: String -> T.Type -> Expression -> Expression
typeBeta arg t target =
  let
    betaCurr = typeBeta arg t
  in
  case target of
    Abstraction argName type' body -> Abstraction argName (substituteType arg t type') $ betaCurr body
    Application left right -> Application (betaCurr left) (betaCurr right)
    TypeAbstraction p exp ->
      if arg == p
        then target
        else TypeAbstraction p $ betaCurr exp
    TypeApplication exp t -> TypeApplication (betaCurr exp) t

    _ -> target

substituteType :: String -> T.Type -> T.Type -> T.Type
substituteType name t (T.Parameter p) = if name == p then t else (T.Parameter p)
substituteType name t (T.Arr left right) = T.Arr (substituteType name t left) (substituteType name t right)
substituteType name t type' = type'