module Untyped.Tokens where


data Token
  = Identifier String
  | Dot
  | Lambda
  | Macro String
  | Operator String
  | Natural Int
  | LeftP
  | RightP
  deriving (Show)