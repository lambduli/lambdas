module SystemF.Types where


data Type
  = Parameter String
  | ForAll String Type
  | Arr Type Type
  | Nat
  | Boolean

instance Show Type where
  show (Parameter p) = p
  show (ForAll name type') = "(forall " ++ name ++ " . " ++ show type' ++ ")"
  show (Arr left@(Arr a b) c) = "(" ++ show left ++ ") -> " ++ show c
  show (Arr a b) = show a ++ " -> " ++ show b
  show Nat = "Nat"
  show Boolean = "Bool"

instance Eq Type where
  (Parameter a) == (Parameter b) = a == b
  Nat == Nat = True
  Boolean == Boolean = True
  (Arr t1 t2) == (Arr t1' t2') = (t1 == t1') && (t2 == t2')
  (ForAll p1 t1) == (ForAll p2 t2) = t1 == t2
  _ == _ = False