{-# LANGUAGE TemplateHaskell, QuasiQuotes #-} 
{-# Language FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Html_Data where

import Data.List
import Language.Haskell.TH 
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax 
import Control.Monad
import Control.Applicative

import Text.ParserCombinators.Parsec hiding((<|>), many)
import Text.ParserCombinators.Parsec.Token hiding (parens)
import Text.ParserCombinators.Parsec.Expr as ParsecExpr

import Helper
import Data

type AttributeName = String

data Placeholder a = Null | ValueB Bool | ValueA Template | P String
    deriving (Eq, Ord)

data AttributeValue a = Value String | Placeholder String (Placeholder a)

data Attribute a =  A AttributeName | Av AttributeName (AttributeValue a) | If (Expr a)

data Element a = EName String [Attribute a] 

data ForWrapper a = FW (Element a) (For a)

data Content a = CText String | CVar String (Placeholder a)

data SingleValue a = Single [(Element a, [HTMLValue a])]

data HTMLValue a = HTML (For a) (SingleValue a)  | HContent (Content a)

data For a = N | F String String Template
    deriving Show

data BoolOp = Eq | Lt | Gt | Le | Ge | Ne
    deriving (Show, Eq, Ord)

data BoolExpr a = BExpr String BoolOp String (Placeholder a) (Placeholder a) | BExprS String (Placeholder a)
    deriving (Show, Eq, Ord)

data Expr a = Not (Expr a) | And (Expr a) (Expr a) | Or (Expr a) (Expr a) | Var (BoolExpr a) | SubExpr (Expr a)
    deriving(Eq, Show)  

instance Show a => Show (Placeholder a) where
    show (Null) = ""
    show (ValueB x) = ""
    show (ValueA x) = show x
    show (P x) = id x

instance Show a => Show (AttributeValue a) where
    show (Value a) = show a
    show (Placeholder a b) = show b

instance Show a => Show (Attribute a) where
    show (A x) = id x
    show (Av x y) = " " ++ id x ++ "=" ++ show y
    show (If x) = ""

instance Show a => Show (Element a) where
    show (EName a b) = id a ++ (list_to_string $ filter (filterAttribute) b)

instance Show a => Show (Content a) where
    show (CText a) = id a
    show (CVar a b) = show b

instance (Show a, Ord a) => Show (SingleValue a) where
    show (Single x) = concat ["<" ++ show x1 ++ ">" ++ (list_to_string $ x2) ++ "</" ++ id (getElemName x1) ++ ">"  | (x1, x2) <- x,  eval x1]

instance (Show a, Ord a) => Show (HTMLValue a) where 
    show (HTML _ x) = show x
    show (HContent x) = show x    

instance Lift (For a) where
    lift (N) = (conE 'N)
    lift (F x y z) = appE (appE (appE (conE 'F) (lift x)) (lift y)) (mkVar y)

instance Lift (HTMLValue a) where
    lift (HTML x y) = appE (appE (conE 'HTML) (lift x)) (mkFor x y)
    lift (HContent i) = appE (conE 'HContent) (lift i)

instance Lift (SingleValue a) where
    lift (Single x) = appE (conE 'Single) (lift x)

instance Lift (Content a) where
    lift (CText i) = appE (conE 'CText) (lift i)
    lift (CVar x y) = appE (appE (conE 'CVar) (lift x)) (appE (conE 'P) (newShow $ mkVar x))

newShow x = appE (varE $ mkName "show") x

instance Lift (Element a) where
    lift (EName x y) = appE (appE (conE 'EName) (lift x)) (lift y)

instance Lift (Attribute a) where
    lift (A i) = appE (conE 'A) (lift i)
    lift (Av x y) = appE (appE (conE 'Av) (lift x)) (lift y)
    lift (If i) = appE (conE 'If) (lift i)

instance Lift (AttributeValue a) where
    lift (Value i) = appE (conE 'Value) (lift i)
    lift (Placeholder x y) = appE (appE (conE 'Placeholder) (lift x)) (appE (conE 'ValueA) (mkVar x))

instance Lift a => Lift (Placeholder a) where
    lift (P i) = appE (conE 'P) (lift i)
    lift (Null) = (conE 'Null)
    lift (ValueB i) = appE (conE 'ValueB) (lift i)
    lift (ValueA i) = appE (conE 'ValueA) (lift i)

instance Lift (Expr a) where
    lift (Not i) = appE (conE 'Not) (lift i)
    lift (And x y) = appE (appE (conE 'And) (lift x)) (lift y)
    lift (Or x y) = appE (appE (conE 'Or) (lift x)) (lift y)
    lift (Var i) = appE (conE 'Var) (lift i)
    lift (SubExpr i) = appE (conE 'SubExpr) (lift i)

instance Lift BoolOp where
    lift (Eq) = conE 'Eq
    lift (Lt) = conE 'Lt
    lift (Gt) = conE 'Gt
    lift (Le) = conE 'Le
    lift (Ge) = conE 'Ge
    lift (Ne) = conE 'Ne

instance Lift (BoolExpr a) where
    lift (BExpr x y z i j) = appE (appE (appE (appE (appE (conE 'BExpr) (lift x)) (lift y)) (lift z)) (appE (conE 'ValueA) (mkVar x))) (appE (conE 'ValueA) (mkVar z))
    lift (BExprS x y) = appE (appE (conE 'BExprS) (lift x)) (appE (conE 'ValueB) (mkVar x))
    

mkFor:: For a -> SingleValue a -> ExpQ
mkFor (F x y z) s = case s of 
    (Single a) -> do
        let multSingle = compE [bindS (varP $ mkName x) (appE unpack_ (varE $ mkName y)), noBindS (appE (varE $ mkName "f") (lift a))]
        let single = appE (varE $ mkName "concat") (multSingle)
        appE (conE 'Single) (single)
        where 
            unpack_ = varE $ mkName "unpack"
mkFor (N) s = case s of 
    (Single a) -> lift s

unpack a = case a of 
    T_List list -> list
    _ -> []

mkVar :: String -> ExpQ
mkVar a = varE $ mkName a


filterAttribute :: (Attribute a) -> Bool
filterAttribute (A x) = True
filterAttribute (Av x y) = True
filterAttribute (If x) = False

getElemName :: (Element a) -> String
getElemName (EName a _)  = a

eval :: Ord a => (Element a) -> Bool
eval (EName a b) = all (\x -> x) (map (\x -> 
    case x of  
        If a -> evalSatis a
        _ -> True
    ) b)

evalSatis :: Ord a => (Expr a) -> Bool
evalSatis (Or e1 e2) = (evalSatis e1 || evalSatis e2)
evalSatis (And e1 e2) = (evalSatis e1 && evalSatis e2)
evalSatis (Var a) = evalBoolean a
evalSatis (SubExpr a) = evalSatis a
evalSatis (Not e) = not (evalSatis e)

evalBoolean :: (Eq a, Ord a) => BoolExpr a -> Bool
evalBoolean (BExpr _ Eq _ x y) = x == y
evalBoolean (BExpr _ Lt _ x y) = x < y
evalBoolean (BExpr _ Gt _ x y) = x > y
evalBoolean (BExpr _ Le _ x y) = x <= y
evalBoolean (BExpr _ Ge _ x y) = x >= y
evalBoolean (BExpr _ Ne _ x y) = x /= y
evalBoolean _ = False