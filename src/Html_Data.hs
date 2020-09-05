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

data Placeholder = Null | ValueB Bool | ValueA Template | P String
    deriving (Eq, Ord)

data AttributeValue = Value String | Placeholder String Placeholder

data Attribute =  A AttributeName | Av AttributeName AttributeValue | If Expr

data Element = EName String [Attribute] 

data ForWrapper = FW Element For

data Content = CText String | CVar String Placeholder

data SingleValue = Single [(Element, [HTMLValue])]

data HTMLValue = HTML For SingleValue  | HContent Content

data For = N | F String String Template
    deriving Show

data BoolOp = Eq | Lt | Gt | Le | Ge | Ne
    deriving (Show, Eq, Ord)

data BoolExpr = BExpr String BoolOp String Placeholder Placeholder | BExprS String Placeholder
    deriving (Show, Eq, Ord)

data Expr = Not Expr | And Expr Expr | Or Expr Expr | Var BoolExpr | SubExpr Expr
    deriving(Eq, Show)  

instance Show Placeholder where
    show (Null) = ""
    show (ValueB x) = ""
    show (ValueA x) = show x
    show (P x) = id x

instance Show AttributeValue where
    show (Value a) = show a
    show (Placeholder a b) = show b

instance Show Attribute where
    show (A x) = id x
    show (Av x y) = " " ++ id x ++ "=" ++ show y
    show (If x) = ""

instance Show Element where
    show (EName a b) = id a ++ (list_to_string $ filter (filterAttribute) b)

instance Show Content where
    show (CText a) = id a
    show (CVar a b) = show b

instance Show SingleValue where
    show (Single x) = concat ["<" ++ show x1 ++ ">" ++ (list_to_string $ x2) ++ "</" ++ id (getElemName x1) ++ ">"  | (x1, x2) <- x,  eval x1]

instance Show HTMLValue where 
    show (HTML _ x) = show x
    show (HContent x) = show x    

instance Lift For where
    lift (N) = (conE 'N)
    lift (F x y z) = appE (appE (appE (conE 'F) (lift x)) (lift y)) (mkVar y)

instance Lift HTMLValue where
    lift (HTML x y) = appE (appE (conE 'HTML) (lift x)) (mkFor x y)
    lift (HContent i) = appE (conE 'HContent) (lift i)

instance Lift SingleValue where
    lift (Single x) = appE (conE 'Single) (lift x)

instance Lift Content where
    lift (CText i) = appE (conE 'CText) (lift i)
    lift (CVar x y) = appE (appE (conE 'CVar) (lift x)) (appE (conE 'P) (newShow $ mkVar x))

newShow x = appE (varE $ mkName "show") x

instance Lift Element where
    lift (EName x y) = appE (appE (conE 'EName) (lift x)) (lift y)

instance Lift Attribute where
    lift (A i) = appE (conE 'A) (lift i)
    lift (Av x y) = appE (appE (conE 'Av) (lift x)) (lift y)
    lift (If i) = appE (conE 'If) (lift i)

instance Lift AttributeValue where
    lift (Value i) = appE (conE 'Value) (lift i)
    lift (Placeholder x y) = appE (appE (conE 'Placeholder) (lift x)) (appE (conE 'ValueA) (mkVar x))

instance Lift Placeholder where
    lift (P i) = appE (conE 'P) (lift i)
    lift (Null) = (conE 'Null)
    lift (ValueB i) = appE (conE 'ValueB) (lift i)
    lift (ValueA i) = appE (conE 'ValueA) (lift i)

instance Lift Expr where
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

instance Lift BoolExpr where
    lift (BExpr x y z i j) = appE (appE (appE (appE (appE (conE 'BExpr) (lift x)) (lift y)) (lift z)) (appE (conE 'ValueA) (mkVar x))) (appE (conE 'ValueA) (mkVar z))
    lift (BExprS x y) = appE (appE (conE 'BExprS) (lift x)) (appE (conE 'ValueB) (mkVar x))
    

mkFor:: For -> SingleValue -> ExpQ
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


filterAttribute :: Attribute -> Bool
filterAttribute (A x) = True
filterAttribute (Av x y) = True
filterAttribute (If x) = False

getElemName :: Element -> String
getElemName (EName a _)  = a

eval :: Element -> Bool
eval (EName a b) = all (\x -> x) (map (\x -> 
    case x of  
        If a -> evalSatis a
        _ -> True
    ) b)

evalSatis :: Expr -> Bool
evalSatis (Or e1 e2) = (evalSatis e1 || evalSatis e2)
evalSatis (And e1 e2) = (evalSatis e1 && evalSatis e2)
evalSatis (Var a) = evalBoolean a
evalSatis (SubExpr a) = evalSatis a
evalSatis (Not e) = not (evalSatis e)

evalBoolean :: BoolExpr -> Bool
evalBoolean (BExpr _ Eq _ x y) = x == y
evalBoolean (BExpr _ Lt _ x y) = x < y
evalBoolean (BExpr _ Gt _ x y) = x > y
evalBoolean (BExpr _ Le _ x y) = x <= y
evalBoolean (BExpr _ Ge _ x y) = x >= y
evalBoolean (BExpr _ Ne _ x y) = x /= y
evalBoolean _ = False

f a = a