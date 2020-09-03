{-# LANGUAGE TemplateHaskell, QuasiQuotes #-} 
{-# Language FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Satisfy where 

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token hiding (parens)
import Text.ParserCombinators.Parsec.Expr as ParsecExpr
import Control.Applicative hiding ((<|>))
import Control.Monad



import Language.Haskell.TH 
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax 


data Placeholder = Null | Value Bool
    deriving (Show, Eq, Ord)

data Expr = Not Expr | And Expr Expr | Or Expr Expr | Var String Placeholder | SubExpr Expr
    deriving(Eq, Show)

instance Lift Expr where
  lift (Not i) = appE (conE 'Not) (lift i)
  lift (And x y) = appE (appE (conE 'And) (lift x)) (lift y)
  lift (Or x y) = appE (appE (conE 'Or) (lift x)) (lift y)
  lift (Var x y) = appE (appE (conE 'Var) (lift x)) (appE (conE 'Value) (mkVar x))
  lift (SubExpr i) = appE (conE 'SubExpr) (lift i)

instance Lift Placeholder where
  lift (Null) = (conE 'Null)
  lift (Value i) = appE (conE 'Value) (lift i)

mkVar :: String -> ExpQ
mkVar a = varE $ mkName a

--parse satisfiability "" "(a OR b AND c) AND (a AND NOT b)"

compile str = do 
    case parse satisfiability "" str of 
        Right (a) -> a
        Left (_) -> error "parse error"

sat = QuasiQuoter {quoteExp  = lift . compile,
    quotePat  = error "no pats for sat",
    quoteType  = error "no type for sat",
    quoteDec  = error "no decs for sat"
}

satisfiability :: Parser Expr
satisfiability = expr
  where expr      = buildExpressionParser operators term <?> "compound expression"
        term      =  parens expr <|> variable <?> "full expression"
        operators = [ [Prefix (string "NOT" >> spaces >> return Not)]
                    , [binary "AND" And]
                    , [binary "OR" Or] ]
          where binary n c = ParsecExpr.Infix (string n *> spaces *> pure c) AssocLeft
        parens p = SubExpr <$> (char '(' *> spaces *> p <* char ')' <* spaces) <?> "parens"

variable :: Parser Expr
variable = do 
  var <- ((some (letter)) <* spaces) <?> "variable"
  return $ Var var Null

eval :: Expr -> Bool
eval (Or e1 e2) = (eval e1 || eval e2)
eval (And e1 e2) = (eval e1 && eval e2)
eval (Var a (Value b)) = b
eval (SubExpr a) = eval a
eval (Not e) = not (eval e)

