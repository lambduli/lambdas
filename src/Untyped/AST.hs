module Untyped.AST where

data Expression
  = Variable String
  | Abstraction String Expression
  | Application Expression Expression
  | Natural Int
  | Macro String
  | Operator String

instance Show Expression where
  show (Variable name) = name
  show (Abstraction arg body) = "(λ " ++ arg ++ " . " ++ show body ++ ")"
  show (Application left (Application rleft rright)) = show left ++ " (" ++ show rleft ++ " " ++ show rright ++ ")"
  show (Application left right) = show left ++ " " ++ show right
  show (Natural int) = show int
  show (Macro str) = str
  show (Operator str) = str