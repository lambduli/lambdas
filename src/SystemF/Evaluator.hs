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
    Application left right ->
      case (normalForm left, normalForm right) of
        (False, _) -> False
        (True, False) -> False
        (True, True)
          | builtInBinary tree -> False
          | builtInUnary left -> False
          | otherwise -> True
    -- Application left right -> normalForm left && normalForm right
    
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

    TypeApplication (TypeAbstraction arg exp) rightT -> typeBeta arg rightT $ typeTAlpha arg (freeTTVar rightT) exp

    TypeApplication left right ->
      TypeApplication (normalStep left) right

    Abstraction arg t body -> Abstraction arg t $ normalStep body

    Application (Abstraction arg _ body) right -> beta arg right $ typeAlpha arg (freeTVar right) $ alpha arg (freeVar right) body

    Application left right ->
      case (normalForm left, normalForm right) of
        (False, _) -> Application (normalStep left) right
        (True, False) -> Application left (normalStep right)
        (True, True)
          | builtInBinary tree -> betaValue tree
          | builtInUnary tree -> betaValue tree
          | otherwise -> Application left right

    -- Application left right ->
    --   if normalForm left
    --     then Application left (normalStep right)
    --     else Application (normalStep left) right

    _ -> tree


builtInBinary :: Expression -> Bool
builtInBinary (Application (Application (Operator op) left) right)
  | isBinary op = True
  | otherwise = False
builtInBinary _ = False

builtInUnary :: Expression -> Bool
builtInUnary (Application (Operator op) right)
  | isUnary op = True
  | otherwise = False
builtInUnary _ = False

isBinary :: String -> Bool
isBinary op
  = elem op ["=", "+", "-", "*", "/", "%", "^", ">=", "<=", "&&", "||"]

isUnary :: String -> Bool
isUnary "!" = True
isUnary _ = False


-- TODO following 3 functions do roughly the same thing - only on different inputs
-- somehow combine them out/together
freeTTVar :: T.Type -> [String]
freeTTVar T.Nat = []
freeTTVar T.Boolean = []
freeTTVar t = freeTVar'' [] [] t
  -- where
  --   freeTTVar' bound free t =
  --     case t of
  --       T.Parameter p -> if elem p bound then free else p : free -- TODO: if p already in free - don't
  --       T.Arr leftT rightT -> (freeTTVar' bound free leftT) ++ (freeTTVar' bound free leftT)
  --       T.ForAll par t'

freeTVar :: Expression -> [String]
freeTVar = freeTVar' [] [] where
  freeTVar' :: [String] -> [String] -> Expression -> [String]
  freeTVar' bound free tree =
    case tree of
      Application left right -> (freeTVar' bound free left) ++ (freeTVar' bound free right) -- duplicities may occur
      TypeApplication left t -> (freeTVar' bound free left) ++ (freeTVar'' bound free t)
      Abstraction arg t body -> (freeTVar'' bound free t) ++ (freeTVar' bound free body)
      TypeAbstraction par exp -> freeTVar' (par : bound) free exp
      _ -> free

freeTVar'' :: [String] -> [String] -> T.Type -> [String]
freeTVar'' bound free t =
  case t of
    T.Parameter name -> if elem name bound then free else name : free
    T.ForAll par t' -> freeTVar'' (par : bound) free t'
    T.Arr left right -> (freeTVar'' bound free left) ++ (freeTVar'' bound free right) -- duplicities may occur
    _ -> free -- Primitive Types

freeVar :: Expression -> Set String
freeVar = freeVar' Set.empty Set.empty where
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


-- TODO: similar situation - next 3 functions solve similar problem just on different inputs/levels
typeTAlpha :: String -> [String] -> Expression -> Expression
typeTAlpha arg free exp =
  let alphaCurr = typeTAlpha arg free
  in
    case exp of
      TypeAbstraction p exp' ->
        if arg == p -- shadowing on type level
          then exp
          else if elem arg (freeTVar exp') && elem p free
            then
              let
                newName = p ++ "'"
                replacement = T.Parameter newName
              in
                TypeAbstraction newName $ alphaCurr $ typeBeta p replacement exp'
          else exp
      Abstraction argument t body -> Abstraction argument t $ alphaCurr body
      Application left right -> Application (alphaCurr left) (alphaCurr right)
      TypeApplication left t -> TypeApplication (alphaCurr left) t
      _ -> exp


typeAlpha :: String -> [String] -> Expression -> Expression
typeAlpha arg free exp =
  let alphaCurr = typeAlpha arg free in
    case exp of
      TypeAbstraction p exp' ->
        if arg == p -- tohle se snad nikdy nemuze stat ne? arg je lowercase a p je uppercase -- OTESTOVAT ruzna exprs jestil kdyz to smazu tak se nic mezneni a analyzovat jesi se to skutecne nikdy nemuze stat
          then exp
          else if elem arg (freeVar exp') && elem p free then
            let
              newName = p ++ "'"
              replacement = T.Parameter newName
              in
                TypeAbstraction newName $ alphaCurr $ typeBeta p replacement exp'
          else exp
      Abstraction argument t body -> Abstraction argument t $ alphaCurr body
      Application left right -> Application (alphaCurr left) (alphaCurr right)
      TypeApplication left t -> TypeApplication (alphaCurr left) t
      _ -> exp


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
          replacement = Variable newName
        in
        Abstraction newName t $ alphaCurr $ beta argument replacement body
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
    Abstraction argName type' body -> Abstraction argName (T.substituteType arg t type') $ betaCurr body
    Application left right -> Application (betaCurr left) (betaCurr right)
    TypeAbstraction p exp ->
      if arg == p
        then target
        else TypeAbstraction p $ betaCurr exp
    TypeApplication exp t' -> TypeApplication (betaCurr exp) $ T.substituteType arg t t'
    _ -> target

-- ["=", "+", "-", "*", "/", "%", "^", ">=", "<=", "&&", "||"]
betaValue :: Expression -> Expression
betaValue (Application (Application (Operator "=") (Natural left)) (Natural right))
  = Boolean $ left == right
betaValue (Application (Application (Operator "+") (Natural left)) (Natural right))
  = Natural $ left + right
betaValue (Application (Application (Operator "-") (Natural left)) (Natural right))
  = Natural $ left - right
betaValue (Application (Application (Operator "*") (Natural left)) (Natural right))
  = Natural $ left * right
betaValue (Application (Application (Operator "/") (Natural left)) (Natural right))
  = Natural $ left `div` right
betaValue (Application (Application (Operator "%") (Natural left)) (Natural right))
  = Natural $ left `mod` right
betaValue (Application (Application (Operator "^") (Natural left)) (Natural right))
  = Natural $ left ^ right
betaValue (Application (Application (Operator ">=") (Natural left)) (Natural right))
  = Boolean $ left >= right
betaValue (Application (Application (Operator "<=") (Natural left)) (Natural right))
  = Boolean $ left <= right
betaValue (Application (Application (Operator "&&") (Boolean left)) (Boolean right))
  = Boolean $ left && right
betaValue (Application (Application (Operator "||") (Boolean left)) (Boolean right))
  = Boolean $ left || right
betaValue (Application (Operator "!") (Boolean left))
  = Boolean $ not left
