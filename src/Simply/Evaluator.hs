module Simply.Evaluator where

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Simply.AST as AST

normalize :: AST.Expression -> AST.Expression
normalize tree =
  if not (normalForm tree) then
    normalize (normalStep tree)
  else
    tree

normalForm :: AST.Expression -> Bool
normalForm tree =
  case tree of
    AST.Abstraction _ _ body -> normalForm body
    AST.Application (AST.Abstraction _ _ _) _ -> False
    AST.Application left right -> normalForm left && normalForm right
    _ -> True
    -- AST.Variable _ -> True
    -- AST.Natural _ -> True
    -- AST.Boolean _ -> True
    -- AST.Macro _ -> True
    -- AST.Operator _ -> True

normalStep :: AST.Expression -> AST.Expression
normalStep tree =
  case tree of
    AST.Abstraction arg t body -> AST.Abstraction arg t (normalStep body)
    AST.Application (AST.Abstraction arg _ body) right -> beta arg right (alpha arg (free right) body)
    AST.Application left right ->
      if normalForm left then
        AST.Application left (normalStep right)
      else AST.Application (normalStep left) right
    _ -> tree

free :: AST.Expression -> Set String
free = free2 Set.empty Set.empty

free2 :: Set String -> Set String -> AST.Expression -> Set String
free2 bound freeVars tree =
  case tree of
    AST.Variable name ->
      if Set.member name bound then
        freeVars
      else
        Set.insert name freeVars
    AST.Abstraction arg _ body -> free2 (Set.insert arg bound) freeVars body
    AST.Application left right ->
      let
        leftFree = free2 bound freeVars left
        rightFree = free2 bound freeVars right
      in
      Set.union leftFree rightFree
    _ -> freeVars -- AST.Number, AST.Boolean, AST.Macro

alpha :: String -> Set String -> AST.Expression -> AST.Expression
alpha arg freeArg tree =
  let
    alphaCurr = alpha arg freeArg
  in
  case tree of
    AST.Abstraction argument t body ->
      if argument == arg then -- shadowing
        tree
      else if Set.member arg (free body) && Set.member argument freeArg then
      -- free original Argument in Body && current Argument in conflict with free Vars from Right Value
        let
          newName = "_" ++ argument ++ "_"
          replacement = AST.Variable ("_" ++ argument ++ "_")
        in
        AST.Abstraction newName t (alphaCurr (beta argument replacement body))
      else tree
    AST.Application left right -> AST.Application (alphaCurr left) (alphaCurr right)
    _ -> tree

beta :: String -> AST.Expression -> AST.Expression -> AST.Expression
beta arg value target =
  let
    betaCurr = beta arg value
  in
  case target of
    AST.Variable name ->
      if arg == name then
        value
      else
        target
    AST.Abstraction argName t body ->
      if arg == argName then
        target
      else
        AST.Abstraction argName t (betaCurr body)
    AST.Application left right -> AST.Application (betaCurr left) (betaCurr right)
    _ -> target
    -- AST.Natural n -> AST.Natural n
    -- AST.Macro t -> AST.Macro t
    -- AST.Boolean b -> AST.Boolean b
    -- AST.Operator op -> AST.Operator op