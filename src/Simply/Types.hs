module Simply.Types where


data Type
  = Var
  | Arr Type Type
  | Nat
  | Boolean
  deriving (Eq)

instance Show Type where
  show (Arr left@(Arr a b) c) = "(" ++ show left ++ ") -> " ++ show c
  show (Arr a b) = show a ++ " -> " ++ show b
  show Nat = "Nat"
  show Boolean = "Bool"