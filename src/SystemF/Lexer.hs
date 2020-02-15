module SystemF.Lexer where

import Data.Char (isUpper, isLower, isDigit)
import Text.ParserCombinators.ReadP

import SystemF.Tokens


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

digit :: ReadP Char
digit =
  satisfy isDigit

isLeftB :: Char -> Bool
isLeftB char = char == '['

isRightB :: Char -> Bool
isRightB char = char == ']'

isLeftP :: Char -> Bool
isLeftP char = char == '('

isRightP :: Char -> Bool
isRightP char = char == ')'

leftParen :: ReadP Token
leftParen = do
  satisfy isLeftP
  return LeftP

rightParen :: ReadP Token
rightParen = do
  satisfy isRightP
  return RightP

leftBracket :: ReadP Token
leftBracket = do
  satisfy isLeftB
  return LeftB

rightBracket :: ReadP Token
rightBracket = do
  satisfy isRightB
  return RightB

dot :: ReadP Token
dot = do
  string "."
  return Dot

isFnLambda :: Char -> Bool
isFnLambda char =
  char == 'λ'
  ||
  char == '\\'

isTpLambda :: Char -> Bool
isTpLambda char =
  char == 'Λ'
  ||
  char == '/'

fnLambda :: ReadP Token
fnLambda = do
  satisfy isFnLambda
  return FunctionLambda

tpLambda :: ReadP Token
tpLambda = do
  satisfy isTpLambda
  return TypeLambda

var :: ReadP Token
var = do
  name <- munch1 isLower
  return $ Identifier name

typeVar :: ReadP Token
typeVar = do
  name <- munch1 isUpper
  return $ TypeVar name

natural :: ReadP Token
natural = do
  digits <- munch1 isDigit
  return $ Natural $ read digits

macro :: ReadP Token
macro = do
  name <- munch1 isUpper
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
  sepBy (choice [macro, operator, natural, var, typeVar, fnLambda, tpLambda, leftParen, rightParen, dot]) skipSpaces