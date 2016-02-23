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

data Result = Result { finalTerm :: Term
                     , reductionCount :: Int
                     , reductionTerms :: [Term]
                     , reductionTypes :: [String]
                     } deriving(Show, Eq) 

-------------------- PARSER --------------------------------

lambdaTerm :: Parser Term
lambdaTerm = lambdaAbstraction <|> lambdaApplication <|> simple

lambdaAbstraction :: Parser Term
lambdaAbstraction = do
      char '\\'
      var <- letter <|> char '$' <|> char '#'
      char '.'
      body <- lambdaTerm
      return(Abstraction [var] body)

lambdaApplication :: Parser Term
lambdaApplication = do
  apps <- many1 simple
  return(foldl1 Application apps)

simple :: Parser Term
simple = lambdaVar <|> paren <|> churchNum <|> encodings

-- Parse positive integers as Church numerals
churchNum :: Parser Term
churchNum = do
  v <- many1 digit
  return(Abstraction "$" (Abstraction "#" (applyTimes (read v :: Int))))

-- Apply the successor function n times
applyTimes 0 = Var "#"
applyTimes n = Application (Var "$") (applyTimes (n-1))

-- Datatype encodings begin with a @ to separate them from variables
encodings :: Parser Term
encodings = do
  char '@'
  datatype <- many letter
  return(myparse (case datatype of
    "succ"  -> "\\n.\\$.\\#.$(n$#)"
    "true"  -> "\\a.\\b.a"
    "false" -> "\\a.\\b.b"
    "if"    -> "\\p.\\a.\\b.pab"
   ))

lambdaVar :: Parser Term
lambdaVar = do
  var <- letter <|> char '$' <|> char '#'
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

-- Try to parse Church numeral
parseChurch (Abstraction "$" (Abstraction "#" term)) = churchToInt term
parseChurch _ = Nothing

-- Parse each $ application as +1, if this stops being a Church numeral return Nothing
churchToInt (Application (Var "$") x) = fmap (+1) (churchToInt x)
churchToInt (Var "#") = Just 0
churchToInt _ = Nothing

ppr :: Term -> PP.Doc
ppr (Var x) = PP.text x
ppr (Abstraction x e) = 
    -- try to parse it as a numeral
    let intRepr = parseChurch (Abstraction x e) in
    -- if we parsed it as a numeral successfully, return the number representation
    case intRepr of
      Just churchInt -> PP.int $ churchInt
    -- else show it as a normal abstraction
      Nothing -> (PP.fcat [(PP.fcat [PP.text "\\",PP.text x,PP.text "."]),(ppr e)])
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

