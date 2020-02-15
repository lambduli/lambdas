module SystemF.Types where

data Type
  = Parameter String
  | Arr Type Type
  | Nat
  | Boolean
  deriving (Eq)

instance Show Type where
  show (Parameter p) = p
  show (Arr left@(Arr a b) c) = "(" ++ show left ++ ") -> " ++ show c
  show (Arr a b) = show a ++ " -> " ++ show b
  show Nat = "Nat"
  show Boolean = "Bool"
--   show (List t) = "[" ++ show t ++ "]"
--   show (Pair a b) = "(" ++ show a ++ ", " ++ show b ++ ")"