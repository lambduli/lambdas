module Simply.Tokens where

data Token
  = Identifier String
  | Dot
  | Lambda
  | Macro String
  | Operator String
  | Natural Int
  | LeftP
  | RightP
  | Arrow
  | Colon
  deriving (Show)