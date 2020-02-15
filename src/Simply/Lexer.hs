module Simply.Lexer where

import Text.ParserCombinators.ReadP

import Simply.Tokens

isSimpleOperator :: Char -> Bool
isSimpleOperator char =
  any (char ==) "=+-*/%^!"

simpleOperator :: ReadP Token
simpleOperator = do
  operator <- satisfy isSimpleOperator
  return $ Operator [operator]

compositeOperator :: ReadP Token
compositeOperator = do
  operator <- choice [string ">=", string "<=", string "&&", string "||"]
  return $ Operator operator

operator :: ReadP Token
operator =
  choice [compositeOperator, simpleOperator]

isDigit :: Char -> Bool
isDigit char =
  char >= '0' && char <= '9'

digit :: ReadP Char
digit =
  satisfy isDigit

isLetter :: Char -> Bool
isLetter char = 
  (char >= 'a' && char <= 'z')
  ||
  (char >= 'A' && char <= 'Z')

letter :: ReadP Char
letter =
  satisfy isLetter

isLeftP :: Char -> Bool
isLeftP char = char == '('

isRightP :: Char -> Bool
isRightP char = char == ')'

-- isParen :: Char -> Bool
-- isParen char =
--   isLeftP char
--   ||
--   isRightP char

leftParen :: ReadP Token
leftParen = do
  satisfy isLeftP
  return LeftP

rightParen :: ReadP Token
rightParen = do
  satisfy isRightP
  return RightP

dot :: ReadP Token
dot = do
  string "."
  return Dot

isLambda :: Char -> Bool
isLambda char =
  char == 'λ'
  ||
  char == '\\'

lambda :: ReadP Token
lambda = do
  satisfy isLambda
  return Lambda

varName :: ReadP Token
varName = do
  name <- munch1 isLetter -- TODO: only lower-case letters for variable names
  return $ Identifier name

number :: ReadP Token
number = do
  digits <- munch1 isDigit
  return $ Natural $ read digits

isMacro :: Char -> Bool
isMacro char =
  char >= 'A' && char <= 'Z'

macro :: ReadP Token
macro = do
  name <- munch1 isMacro
  return $ Macro name

colon :: ReadP Token
colon = do
  string ":"
  return Colon

arr :: ReadP Token
arr = do
  string "->"
  return Arrow

lexer :: ReadP [Token]
lexer =
  sepBy (choice [macro, operator, number, varName, lambda, leftParen, rightParen, dot]) skipSpaces