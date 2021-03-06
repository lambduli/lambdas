module Simply.Tokens where


data Token
  = Identifier String
  | Dot
  | Lambda
  | Operator String
  | Natural Int
  | Boolean Bool
  | LeftP
  | RightP
  | Arrow
  | Colon
  deriving (Show)