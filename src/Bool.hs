{-# LANGUAGE TemplateHaskell, QuasiQuotes #-} 
{-# Language FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Bool where 

import Data.List
import Language.Haskell.TH 
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax 
import Control.Monad


import Text.ParserCombinators.Parsec hiding((<|>), many)
import Control.Applicative
import Helper

data BoolOp = Eq | Lt | Gt | Le | Ge | Ne
    deriving (Show)


data Placeholder a = Null | Value a
    deriving (Show, Eq, Ord)


data BoolExpr a = BExpr String BoolOp String (Placeholder a) (Placeholder a)
    deriving (Show)


instance Lift BoolOp where
    lift (Eq) = conE 'Eq
    lift (Lt) = conE 'Lt
    lift (Gt) = conE 'Gt
    lift (Le) = conE 'Le
    lift (Ge) = conE 'Ge
    lift (Ne) = conE 'Ne

instance Lift (BoolExpr a) where
    lift (BExpr x y z i j) = appE (appE (appE (appE (appE (conE 'BExpr) (lift x)) (lift y)) (lift z)) (appE (conE 'Value) (mkVar x))) (appE (conE 'Value) (mkVar z))

instance Lift a => Lift (Placeholder a) where
    lift (Null) = (conE 'Null)
    lift (Value i) = appE (conE 'Value) (lift i)


mkVar :: String -> ExpQ
mkVar a = varE $ mkName a

compile str = do 
    case parse boolExpr "" str of 
        Right (a) -> a
        Left (_) -> error "parse error"

bool = QuasiQuoter {quoteExp  = lift . compile,
    quotePat  = error "no pats for bool",
    quoteType  = error "no type for bool",
    quoteDec  = error "no decs for bool"
}



-- parse boolExpr "" "myVar == myVar2"
boolExpr :: Parser (BoolExpr a)
boolExpr = do
    var1 <- some (letter <|> digit)
    ws
    op <- boolOp 
    ws
    var2 <- some (letter <|> digit)
    return $ BExpr var1 op var2 Null Null


boolOp :: Parser BoolOp
boolOp = string "==" *> pure Eq 
    <|>  string "<" *> pure Lt
    <|>  string ">" *> pure Gt
    <|>  string "<=" *> pure Le
    <|>  string ">=" *> pure Ge
    <|>  string "!=" *> pure Ne
    


eval :: (Eq a, Ord a) => BoolExpr a -> Bool
eval (BExpr _ Eq _ x y) = x == y
eval (BExpr _ Lt _ x y) = x < y
eval (BExpr _ Gt _ x y) = x > y
eval (BExpr _ Le _ x y) = x <= y
eval (BExpr _ Ge _ x y) = x >= y
eval (BExpr _ Ne _ x y) = x /= y
eval _ = False
