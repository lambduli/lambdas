module SystemF.AST where

import Data.Set (Set)
import qualified Data.Set as Set

import qualified SystemF.Types as T


data Expression
  = Variable String
  | TypeAbstraction String Expression
  | Abstraction String T.Type Expression
  | Application Expression Expression
  | TypeApplication Expression T.Type
  | Natural Int
  | Macro String
  | Operator String
  | Boolean Bool

instance Show Expression where
  show (Variable name) = name
  show (TypeAbstraction param body) = "(Λ " ++ param ++ " . " ++ show body ++ ")"
  show (Abstraction arg t body) = "(λ " ++ arg ++ " : " ++ show t ++ " . " ++ show body ++ ")"
  show (Application left (Application rleft rright)) = "(" ++ show left ++ " (" ++ show rleft ++ " " ++ show rright ++ "))"
  show (Application left right) = show left ++ " " ++ show right
  
  show (TypeApplication left right) = show left ++ " [" ++ show right ++ "]" -- may need parens around

  show (Natural n) = show n
  show (Macro str) = str
  show (Operator op) = op
  show (Boolean b) = show b