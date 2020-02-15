module SystemF.Evaluator where

import Data.Set (Set)
import qualified Data.Set as Set

import qualified SystemF.AST as AST
import qualified SystemF.Types as Type

normalize :: AST.Expression -> AST.Expression
normalize tree =
  if not (normalForm tree) then
    normalize (normalStep tree)
  else
    tree

normalForm :: AST.Expression -> Bool
normalForm tree =
  case tree of
    AST.TypeAbstraction _ body -> normalForm body

    AST.TypeApplication (AST.TypeAbstraction _ _) _ -> False
    AST.TypeApplication exp _ -> normalForm exp
    
    AST.Abstraction _ _ body -> normalForm body
    
    AST.Application (AST.Abstraction _ _ _) _ -> False
    AST.Application left right -> normalForm left && normalForm right
    
    _ -> True
    -- AST.Variable _ -> True
    -- AST.Number _ -> True
    -- AST.Macro _ -> True
    -- AST.Operator _ -> True
    -- AST.Boolean _ -> True

normalStep :: AST.Expression -> AST.Expression
normalStep tree =
  case tree of
    AST.TypeAbstraction arg exp -> AST.TypeAbstraction arg $ normalStep exp

    AST.TypeApplication (AST.TypeAbstraction arg exp) rightT -> typeBeta arg rightT exp

    AST.Abstraction arg t body -> AST.Abstraction arg t $ normalStep body
    
    AST.Application (AST.Abstraction arg _ body) right -> beta arg right (alpha arg (freeVar right) body)
    
    AST.Application left right ->
      if normalForm left
        then AST.Application left (normalStep right)
        else AST.Application (normalStep left) right
    
    _ -> tree

freeVar :: AST.Expression -> Set String
freeVar = freeVar' Set.empty Set.empty

freeVar' :: Set String -> Set String -> AST.Expression -> Set String
freeVar' bound freeVars tree =
  case tree of
    AST.Variable name ->
      if Set.member name bound then
        freeVars
      else
        Set.insert name freeVars
    AST.Abstraction arg _ body -> freeVar' (Set.insert arg bound) freeVars body
    AST.Application left right ->
      let
        leftFree = freeVar' bound freeVars left
        rightFree = freeVar' bound freeVars right
      in
      Set.union leftFree rightFree
    AST.TypeAbstraction p exp -> freeVar' bound freeVars exp
    AST.TypeApplication left t -> freeVar' bound freeVars left
    _ -> freeVars -- AST.Number, AST.Boolean, AST.Macro, AST.Operator

alpha :: String -> Set String -> AST.Expression -> AST.Expression
alpha arg freeArg tree =
  let
    alphaCurr = alpha arg freeArg
  in
  case tree of
    AST.Abstraction argument t body ->
      if arg == argument then -- shadowing
        tree
      else if Set.member arg (freeVar body) && Set.member argument freeArg then
      -- freeVar original Argument in Body && current Argument in conflict with freeVar Vars from Right Value
        let
          newName = "_" ++ argument ++ "_"
          replacement = AST.Variable ("_" ++ argument ++ "_")
        in
        AST.Abstraction newName t (alphaCurr (beta argument replacement body))
      else tree
    AST.Application left right -> AST.Application (alphaCurr left) (alphaCurr right)
    AST.TypeAbstraction p exp -> AST.TypeAbstraction p $ alphaCurr exp
    AST.TypeApplication left t -> AST.TypeApplication (alphaCurr left) t
    _ -> tree

beta :: String -> AST.Expression -> AST.Expression -> AST.Expression
beta arg value target =
  let
    betaCurr = beta arg value
  in
  case target of
    AST.Variable name ->
      if arg == name
        then value
        else target
    AST.Abstraction argName t body ->
      if arg == argName
        then target
        else AST.Abstraction argName t (betaCurr body)
    AST.Application left right -> AST.Application (betaCurr left) (betaCurr right)
    AST.TypeAbstraction p exp -> AST.TypeAbstraction p $ betaCurr exp
    AST.TypeApplication exp t -> AST.TypeApplication (betaCurr exp) t
    _ -> target
    -- AST.Number n -> AST.Number n
    -- AST.Macro t -> AST.Macro t
    -- AST.Boolean b -> AST.Boolean b
    -- AST.Operator op -> AST.Operator op

typeBeta :: String -> Type.Type -> AST.Expression -> AST.Expression
typeBeta arg t target =
  let
    betaCurr = typeBeta arg t
  in
  case target of
    AST.Abstraction argName type' body -> AST.Abstraction argName (substituteType arg t type') $ betaCurr body
    AST.Application left right -> AST.Application (betaCurr left) (betaCurr right)
    AST.TypeAbstraction p exp ->
      if arg == p
        then target
        else AST.TypeAbstraction p $ betaCurr exp
    AST.TypeApplication exp t -> AST.TypeApplication (betaCurr exp) t

    _ -> target

substituteType :: String -> Type.Type -> Type.Type -> Type.Type
substituteType name t (Type.Parameter p) = if name == p then t else (Type.Parameter p)
substituteType name t (Type.Arr left right) = Type.Arr (substituteType name t left) (substituteType name t right)
substituteType name t type' = type'