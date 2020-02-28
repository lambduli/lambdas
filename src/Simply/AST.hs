module Simply.AST where

import qualified Simply.Types as T


data Expression
  = Variable String
  | Abstraction String T.Type Expression
  | Application Expression Expression
  | Natural Int
  | Macro String
  | Operator String
  | Boolean Bool

instance Show Expression where
  show (Variable name) = name
  show (Abstraction arg t body) = "(λ " ++ arg ++ " : " ++ show t ++ " . " ++ show body ++ ")"
  show (Application left (Application rleft rright)) = "(" ++ show left ++ " (" ++ show rleft ++ " " ++ show rright ++ "))"
  show (Application left right) = show left ++ " " ++ show right
  show (Natural n) = show n
  show (Macro str) = str
  show (Operator op) = op
  show (Boolean b) = show b