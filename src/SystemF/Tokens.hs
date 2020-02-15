module SystemF.Tokens where


data Token
  = Identifier String
  | TypeVar String
  | Dot
  | FunctionLambda
  | TypeLambda
  | Macro String
  | Operator String
  | Natural Int
  | LeftP
  | RightP
  | LeftB
  | RightB
  | Arrow
  | Colon
  deriving (Show)