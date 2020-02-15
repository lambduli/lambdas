module Untyped.Parser where

import Text.ParserCombinators.ReadP
import qualified Untyped.AST as AST
import Untyped.Tokens
import Untyped.Lexer


variable :: ReadP AST.Expression
variable = do
  space <- skipSpaces
  id <- var
  case id of Identifier name -> return $ AST.Variable name

natural' :: ReadP AST.Expression
natural' = do
  space <- skipSpaces
  num <- natural
  case num of Natural int -> return $ AST.Natural int

macro' :: ReadP AST.Expression
macro' = do
  space <- skipSpaces
  m <- macro
  case m of Macro str -> return $ AST.Macro str

operator' :: ReadP AST.Expression
operator' = do
  space <- skipSpaces
  op <- operator
  case op of Operator str -> return $ AST.Operator str

application :: ReadP AST.Expression
application = do
  left <- choice [wrapped, variable, operator', macro', natural', abstraction]
  ids <- many1 $ choice [wrapped, variable, operator', macro', natural', abstraction]
  return $ foldl (\exp id -> AST.Application exp id) left ids

abstraction :: ReadP AST.Expression
abstraction = do
  space <- skipSpaces
  l <- leftParen
  space <- skipSpaces
  lam <- lambda
  space <- skipSpaces
  arg <- var
  space <- skipSpaces
  d <- dot
  space <- skipSpaces
  exp <- expression
  space <- skipSpaces
  r <- rightParen
  case arg of Identifier name -> return $ AST.Abstraction name exp

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

expression :: ReadP AST.Expression
expression =  
  choice [variable, natural', macro', operator', application, abstraction, wrapped]