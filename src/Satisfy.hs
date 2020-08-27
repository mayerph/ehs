{-# Language FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Satisfy where 

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token hiding (parens)
import Text.ParserCombinators.Parsec.Expr
import Control.Applicative hiding ((<|>))
import Control.Monad
import Prelude hiding (not)

data Expr = Not Expr | And Expr Expr | Or Expr Expr | Var Char | SubExpr Expr
    deriving(Eq, Show)


--parse parseBoolean "" "(a OR b AND c) AND (a AND NOT b)"

satisfiability :: Parser Expr
satisfiability = expr
  where expr      = buildExpressionParser operators term <?> "compound expression"
        term      =  parens expr <|> variable <?> "full expression"
        operators = [ [Prefix (string "NOT" >> spaces >> return Not)]
                    , [binary "AND" And]
                    , [binary "OR" Or] ]
          where binary n c = Infix (string n *> spaces *> pure c) AssocLeft
        variable = Var     <$> (letter <* spaces)                              <?> "variable"
        parens p = SubExpr <$> (char '(' *> spaces *> p <* char ')' <* spaces) <?> "parens"

