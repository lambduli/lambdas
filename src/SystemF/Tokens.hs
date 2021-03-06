module SystemF.Tokens where


data Token
  = Identifier String
  | TypeVar String
  | Dot
  | FunctionLambda
  | TypeLambda
  -- | Macro String
  | Operator String
  | Natural Int
  | Boolean Bool
  | LeftP
  | RightP
  | LeftB
  | RightB
  | Arrow
  | Colon
  | Forall
  deriving (Show)