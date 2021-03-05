module Simply.Parser where

import Text.ParserCombinators.ReadP

import qualified Simply.AST as AST
import qualified Simply.Types as T
import Simply.Tokens
import Simply.Lexer


-- Type := Var
--       | (Type -> Type)
--       | Type -> Type [-> Type]
--       | (Type -> Type [-> Type])

tNat :: ReadP T.Type
tNat = do
  space <- skipSpaces
  string "Nat"
  return T.Nat

tBool :: ReadP T.Type
tBool = do
  space <- skipSpaces
  string "Bool"
  return T.Boolean

leftPart :: ReadP T.Type
leftPart = do
  left <- choice [tNat, tBool, wrapTArr]
  space <- skipSpaces
  arrow <- arr
  return $ left

tArr :: ReadP T.Type
tArr = do
  tps <- many1 $ choice [leftPart, tNat, tBool, wrapTArr] -- ...leftPart : one of the others : []
  return $ foldr (\tx ta -> T.Arr tx ta) (last tps) (init tps)

wrapTArr :: ReadP T.Type
wrapTArr = do
  s <- skipSpaces
  l <- leftParen
  arr <- tArr
  s <- skipSpaces
  r <- rightParen
  return arr

typeAnnotation :: ReadP T.Type
typeAnnotation =
  choice [tNat, tBool, wrapTArr, tArr]
  

-- EXP := VAR
--      | EXP EXP
--      | (λ VAR . EXP)
--      | NUMBER
--      | MACRO
--      | (EXP)

var :: ReadP AST.Expression
var = do
  space <- skipSpaces
  id <- varName
  case id of Identifier name -> return $ AST.Variable name

num :: ReadP AST.Expression
num = do
  space <- skipSpaces
  number <- number
  case number of Natural int -> return $ AST.Natural int

mac :: ReadP AST.Expression
mac = do
  space <- skipSpaces
  macro <- macro
  case macro of
    Macro "True" -> return $ AST.Boolean True
    Macro "False" -> return $ AST.Boolean False
    Macro str -> return $ AST.Macro str

oper :: ReadP AST.Expression
oper = do
  space <- skipSpaces
  op <- operator
  case op of Operator str -> return $ AST.Operator str

app :: ReadP AST.Expression
app = do
  left <- choice [wrapped, var, oper, mac, num, abst]
  ids <- many1 $ choice [wrapped, var, oper, mac, num, abst]
  return $ foldl (\exp id -> AST.Application exp id) left ids

abst :: ReadP AST.Expression
abst = do
  space <- skipSpaces
  l <- leftParen
  space <- skipSpaces
  lam <- lambda
  space <- skipSpaces
  arg <- varName
  space <- skipSpaces
  dd <- colon
  space <- skipSpaces
  type' <- typeAnnotation
  space <- skipSpaces
  d <- dot
  space <- skipSpaces
  exp <- expression
  space <- skipSpaces
  r <- rightParen
  case arg of Identifier name -> return $ AST.Abstraction name type' exp

wrapped :: ReadP AST.Expression
wrapped = do
  space <- skipSpaces
  l <- leftParen
  space <- skipSpaces
  exp <- expression
  space <- skipSpaces
  r <- rightParen
  space <- skipSpaces
  return exp

-- TODO: take care of exprs: A B C --> (A B) C -- DONE
-- TODO: take care of exprs: (λ a : Int -> Boolean, b : Boolean -> Char, c : Int . b (a c))
expression :: ReadP AST.Expression
expression =  
  choice [var, num, mac, oper, app, abst, wrapped]