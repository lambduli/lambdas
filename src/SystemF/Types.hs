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

-- what about (forall A . Nat -> A -> Nat) == (forall B . Nat -> B -> Nat)
-- example: (\ a : forall A . A -> A -> A . a) (/ B . (\t : B . (\f : B . t)))
instance Eq Type where
  (Parameter a) == (Parameter b) = a == b
  Nat == Nat = True
  Boolean == Boolean = True
  (Arr t1 t2) == (Arr t1' t2') = (t1 == t1') && (t2 == t2')
  (ForAll p1 t1) == (ForAll p2 t2) = t1 == substituteType p2 (Parameter p1) t2
  _ == _ = False

  -- TODO: should there be case for ForAll ???
substituteType :: String -> Type -> Type -> Type
substituteType name t (Parameter p) = if name == p then t else (Parameter p)
substituteType name t (Arr left right) = Arr (substituteType name t left) (substituteType name t right)
substituteType name t (ForAll p t') = if name == p then ForAll p t' else ForAll p $ substituteType name t t'
substituteType name t type' = type'