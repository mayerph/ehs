{-# LANGUAGE TemplateHaskell, QuasiQuotes #-} 
{-# Language FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Satisfiability where 

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token hiding (parens)
import Text.ParserCombinators.Parsec.Expr as ParsecExpr
import Control.Applicative hiding ((<|>))
import Control.Monad
import Bool
import Helper



import Language.Haskell.TH 
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax 

data BoolOp = Eq | Lt | Gt | Le | Ge | Ne
    deriving (Show, Eq, Ord)

data BoolExpr a = BExpr String BoolOp String (Placeholder a) (Placeholder a) | BExprS String (Placeholder a)
    deriving (Show, Eq, Ord)

data Placeholder a = Null | ValueB Bool | Value a
    deriving (Show, Eq, Ord)

data Expr a = Not (Expr a) | And (Expr a) (Expr a) | Or (Expr a) (Expr a) | Var (BoolExpr a) | SubExpr (Expr a)
    deriving(Eq, Show)

instance Lift (Expr a) where
  lift (Not i) = appE (conE 'Not) (lift i)
  lift (And x y) = appE (appE (conE 'And) (lift x)) (lift y)
  lift (Or x y) = appE (appE (conE 'Or) (lift x)) (lift y)
  lift (Var i) = appE (conE 'Var) (lift i)
  lift (SubExpr i) = appE (conE 'SubExpr) (lift i)

instance Lift a => Lift (Placeholder a) where
  lift (Null) = (conE 'Null)
  lift (ValueB i) = appE (conE 'ValueB) (lift i)
  lift (Value i) = appE (conE 'Value) (lift i)

instance Lift BoolOp where
    lift (Eq) = conE 'Eq
    lift (Lt) = conE 'Lt
    lift (Gt) = conE 'Gt
    lift (Le) = conE 'Le
    lift (Ge) = conE 'Ge
    lift (Ne) = conE 'Ne

instance Lift (BoolExpr a) where
    lift (BExpr x y z i j) = appE (appE (appE (appE (appE (conE 'BExpr) (lift x)) (lift y)) (lift z)) (appE (conE 'Value) (mkVar x))) (appE (conE 'Value) (mkVar z))
    lift (BExprS x y) = appE (appE (conE 'BExprS) (lift x)) (appE (conE 'ValueB) (mkVar x))

mkVar :: String -> ExpQ
mkVar a = varE $ mkName a

--parse satisfiability "" "(a OR b AND c) AND (a AND NOT b)"

compile str = do 
    case parse satisfiability "" str of 
        Right (a) -> a
        Left (_) -> error "parse error"

satis = QuasiQuoter {quoteExp  = lift . compile,
    quotePat  = error "no pats for sat",
    quoteType  = error "no type for sat",
    quoteDec  = error "no decs for sat"
}

satisfiability :: Parser (Expr a)
satisfiability = expr
  where expr      = buildExpressionParser operators term <?> "compound expression"
        term      =  parens expr <|> variable <?> "full expression"
        operators = [ [Prefix (string "NOT" >> spaces >> return Not)]
                    , [binary "AND" And]
                    , [binary "OR" Or] ]
          where binary n c = ParsecExpr.Infix (string n *> spaces *> pure c) AssocLeft
        parens p = SubExpr <$> (char '(' *> spaces *> p <* char ')' <* spaces) <?> "parens"

-- testSatis = [sat|d AND e|]
variable :: Parser (Expr a)
variable = do 
  a <- boolExpr
  return $ Var a



--evalSatis :: (Expr a) -> Bool
--evalSatis (Or e1 e2) = (evalSatis e1 || evalSatis e2)
--evalSatis (And e1 e2) = (evalSatis e1 && evalSatis e2)
--evalSatis (Var a) = evalBoolean a
--evalSatis (SubExpr a) = evalSatis a
--evalSatis (Not e) = not (evalSatis e)

opString :: Parser String
opString = string "AND"

boolExpr :: Parser (BoolExpr a)
boolExpr = (try boolExprM) <|> (try boolExprS)

boolExprM :: Parser (BoolExpr a)
boolExprM = do
    --notFollowedBy opString
    var1 <- some (letter <|> digit)
    ws
    op <- boolOp 
    ws
    var2 <- some (letter <|> digit)
    ws
    return $ BExpr var1 op var2 Null Null

boolExprS :: Parser (BoolExpr a)
boolExprS = do 
    a <- ((some (letter)) <* ws) <?> "variable"
    return $ BExprS a Null


boolOp :: Parser BoolOp
boolOp = string "==" *> pure Eq 
    <|>  string "<" *> pure Lt
    <|>  string ">" *> pure Gt
    <|>  string "<=" *> pure Le
    <|>  string ">=" *> pure Ge
    <|>  string "!=" *> pure Ne
    


evalBoolean :: (Eq a, Ord a) => BoolExpr a -> Bool
evalBoolean (BExpr _ Eq _ x y) = x == y
evalBoolean (BExpr _ Lt _ x y) = x < y
evalBoolean (BExpr _ Gt _ x y) = x > y
evalBoolean (BExpr _ Le _ x y) = x <= y
evalBoolean (BExpr _ Ge _ x y) = x >= y
evalBoolean (BExpr _ Ne _ x y) = x /= y
evalBoolean _ = False

