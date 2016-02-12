module Parser where

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.List
import Data.Char
import Text.Parsec
import Text.Parsec.String
import qualified Text.PrettyPrint as PP

data Term = Var String
		  | Application Term Term
		  | Abstraction String Term
		  deriving(Show,Eq)

data Result = Res Term Int [Term] [String] deriving(Show,Eq)

-------------------- PARSER --------------------------------

lambdaTerm :: Parser Term
lambdaTerm = lambdaAbstraction <|> lambdaApplication <|> simple

lambdaAbstraction :: Parser Term
lambdaAbstraction = do
  	char '\\'
  	var <- letter
  	char '.'
  	body <- lambdaTerm
  	return(Abstraction [var] body)

lambdaApplication :: Parser Term
lambdaApplication = do
  apps <- many1 simple
  return(foldl1 Application apps)

simple :: Parser Term
simple = lambdaVar <|> paren

lambdaVar :: Parser Term
lambdaVar = do
  var <- letter
  return(Var [var])

paren :: Parser Term
paren = do
  char '('
  term <- lambdaTerm
  char ')'
  return term

myparse :: String -> Term
myparse str = case (parse lambdaTerm "" str) of
	Left msg -> error $ show msg
	Right term' -> term'

test = myparse "\\z.(\\f.\\x.fzx)(\\y.y)"
pair = myparse "\\x.\\y.\\z.zxy"

----------------------- PRETTY PRINT ------------------------

ppr :: Term -> PP.Doc
ppr (Var x) = PP.text x
ppr (Abstraction x e) = PP.fcat [(PP.fcat [PP.text "\\",PP.text x,PP.text "."]),(ppr e)]
ppr apply = PP.fcat (map parenApp (args apply))


args (Application x y) = args x ++ [y]
args x = [x]

parenApp (x@(Application _ _)) = PP.parens (ppr x)
parenApp (x@(Abstraction _ _)) = PP.parens (ppr x)
parenApp x = ppr x

prettyprint :: Term -> String
prettyprint term = PP.render (ppr term)

------------------------ TEST CASES ------------------------

inputString = "(\\x.\\y.x)(\\z.z)"

parseInputString = myparse inputString

myterm = Application (Abstraction "x" ( Abstraction "y"  (Var "x"))) (Abstraction "z" ( Var "z"))

prettyPrinted = prettyprint myterm







