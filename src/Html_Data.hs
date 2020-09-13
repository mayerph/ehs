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
import Data.Typeable

type AttributeName = String

{-|
  The 'Placeholder' datatype is the haskell internal datatype for a placeholder.
-}
data Placeholder = 
        Null  -- ^ used while parsing 
    |   ValueB Bool -- ^ placeholder for data of the type Boolean
    |   ValueA Template -- ^ placeholder for data of the type Template
    |   P String -- ^ placeholder for data of the type Template
    deriving (Eq, Ord)

{-|
  The 'AttributeValue' datatype is the haskell internal datatype for the value of a html attribute.
-}
data AttributeValue = 
        Value String -- ^ static value 
    |   Placeholder -- ^ dynamic value based on a placeholder
            String -- ^ name/key of the placeholder
            Placeholder -- ^ value of the placeholder
    |   PlaceholderM  -- ^ dynamic value based on a placeholder
            [String] -- ^ name/key of the placeholder and applied constructor an functions
            Placeholder -- ^ value of the placeholder

{-|
  The 'Attribute' datatype is the haskell internal datatype for a html attribute.
-}
data Attribute =  
        A AttributeName -- ^ attribute without a value
    |   Av AttributeName AttributeValue -- ^ attribute with a value
    |   If Expr -- ^ boolean expression 

{-|
  The 'Element' datatype is the haskell internal datatype for the meta information of a html element.
  It has a name (e.g. div) and some attributes (class, id, hIf)
-}
data Element = EName String [Attribute] 

{-|
  The 'ForWrapper' datatype is a Wrapper Element which simplifies the parsing procedure.  
-}
data ForWrapper = FW Element For

{-|
  The 'Content' datatype is the haskell internal datatype for the content of a html Element.
-}
data Content = 
        CText String -- ^ static content 
    |   CVar -- ^ dynamic content based on a placeholder
            String -- ^ name/key of the placeholder
            Placeholder -- ^ value of the placeholder 
    |   CVarM -- ^ dynamic content based on a placeholder
            [String] -- ^ name/key of the placeholder and applied constructor an functions
            Placeholder -- ^ value of the placeholder 

{-|
  The 'SingleValue' datatype is the haskell internal datatype for a single html element with all its content and children.
  The Single constructor contains a list because an iteration pattern can multiply the appearance of a html element. 
-}
data SingleValue = Single [(Element, [HTMLValue])]

{-|
  The 'HTMLValue' datatype is the haskell internal datatype for the whole html document starting with a root element.
-}
data HTMLValue = 
        HTML -- ^ html element 
            For -- ^ iteration pattern for a html element
            SingleValue -- ^ the html element itself
    |   HContent Content -- ^ content (static or dynamic) of a html element

{-|
  The 'For' datatype is the haskell internal datatype for the iteration pattern of a html element. 
-}
data For = 
        N -- ^ constructor if there isn't an iteration pattern
    |   F -- ^ constructor for an iteration pattern
            String -- ^ name/key of a list entry
            String -- ^ name/key of the list
            Template -- ^ value of the list
    |   FM -- ^ constructor for an iteration pattern
            [String] -- ^ name/key of a list entry with its applied constructor and functions
            String -- ^ name/key of the list
            Template -- ^ value of the list
    deriving Show

{-|
  The 'BoolOp' datatype is the haskell internal datatype for a boolean comparison operator. 
-}
data BoolOp = Eq | Lt | Gt | Le | Ge | Ne
    deriving (Show, Eq, Ord)

{-|
  The 'BoolExpr' datatype is the haskell internal datatype for a single boolean expression. 
-}
data BoolExpr = 
        BExpr -- ^ constructor for a boolean expression
            [String] -- ^ name/key of the right term of a boolean expression with its applied constructor and functions
            BoolOp -- ^ boolean comparison operator
            [String] -- ^ name/key of the left term of a boolean expression with its applied constructor and functions
            Placeholder -- ^ value of the right term of a boolean expression
            Placeholder -- ^ value of the left term of a boolean expression
    |   BExprS -- ^ constructor for a placeholder based boolean expression
            String -- ^ name/key of the placeholder
            Placeholder -- ^ value of the placeholder
    deriving (Show, Eq, Ord)

{-|
  The 'Expr' datatype is the haskell internal datatype for boolean expressions combined with logical operators. 
-}
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
    show (PlaceholderM a b) = "\"" ++ show b ++ "\""
    
instance Show Attribute where
    show (A x) = id x
    show (Av x y) = " " ++ id x ++ "=" ++ show y
    show (If x) = ""

instance Show Element where
    show (EName a b) = id a ++ (list_to_string $ filter (filterAttribute) b)

instance Show Content where
    show (CText a) = id a
    show (CVar a b) = show b
    show (CVarM a b) = show b 

instance Show SingleValue where
    show (Single x) = concat ["<" ++ show x1 ++ ">" ++ (list_to_string $ x2) ++ "</" ++ id (getElemName x1) ++ ">"  | (x1, x2) <- x,  eval x1]

instance Show HTMLValue where 
    show (HTML _ x) = show x
    show (HContent x) = show x

{-|
    'Lift For' important snippets:
    1. (mkVar y), 
        - where y is the name of the variable which value is the list of the iteration pattern
        - creates the reference to the variable y. 
        - e.g. [a <- my_list]. Here it would be the reference to my_list      
-}
instance Lift For where
    lift (N) = (conE 'N)
    lift (F x y z) = appE (appE (appE (conE 'F) (lift x)) (lift y)) (mkVar y)
    
{-|
    'Lift HTMLValue' important snippets:
    1. (mkFor x y), 
        - where x is the iteration pattern for a html element and y the html element itself
        - multiplies the html element ragarding to the iteration pattern
-}
instance Lift HTMLValue where
    lift (HTML x y) = appE (appE (conE 'HTML) (lift x)) (mkFor x y)
    lift (HContent i) = appE (conE 'HContent) (lift i)

instance Lift SingleValue where
    lift (Single x) = appE (conE 'Single) (lift x)

{-|
    'Lift Content' important snippets:
    1. (appE (conE 'P) (newShow $ mkVar x))
        - where x is the name of the variable which value is the content of a html element 
        - creates the reference to the variable x. 
        - makes the referenced value to the value of the placeholder

    2. (appE (conE 'ValueA) (mkVarExtension x)), 
        - where x contains the name of the variable hich value is the content of a html element,
            the name of the applied template constructor and applied functions
        - creates the reference to the variable inside of x. 
        - makes the referenced value to the value of the placeholder
-}
instance Lift Content where
    lift (CText i) = appE (conE 'CText) (lift i)
    lift (CVar x y) = appE (appE (conE 'CVar) (lift x)) (appE (conE 'P) (newShow $ mkVar x))
    lift (CVarM x y) = appE (appE (conE 'CVarM) (lift x)) (appE (conE 'ValueA) (mkVarExtension x))

instance Lift Element where
    lift (EName x y) = appE (appE (conE 'EName) (lift x)) (lift y)

instance Lift Attribute where
    lift (A i) = appE (conE 'A) (lift i)
    lift (Av x y) = appE (appE (conE 'Av) (lift x)) (lift y)
    lift (If i) = appE (conE 'If) (lift i)

{-|
    'Lift AttributeValue' important snippets:
    1. (appE (conE 'ValueA) (mkVar x)
        - where x is the name/key of the variable which value is the value of the attribute.
        - creates the reference to the variable x. 
        - makes the referenced value to the value of the placeholder

    2. (appE (conE 'ValueA) (mkVarExtension x)), 
        - where x contains the name of the variable which value is the value of the attribute,
            the name of the applied template constructor and applied functions.
        - creates the reference to the variable inside of x. 
        - makes the referenced value to the value of the placeholder
-}
instance Lift AttributeValue where
    lift (Value i) = appE (conE 'Value) (lift i)
    lift (Placeholder x y) = appE (appE (conE 'Placeholder) (lift x)) (appE (conE 'ValueA) (mkVar x))
    lift (PlaceholderM x y) = appE (appE (conE 'PlaceholderM) (lift x)) (appE (conE 'ValueA) (mkVarExtension x))

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


{-|
    'Lift BoolExpr' important snippets:
    1. (appE (conE 'ValueA) (mkVarExtension x)) 
        - where x contains the name/key of the variable which value should be the left part of a boolean expression
            and the name of the applied template constructor and applied functions
        - creates the reference to the variable inside of x. 
        - makes the referenced value to the value of the placeholder

    2. (appE (conE 'ValueA) (mkVarExtension z))
        - where z is the name/key of the variable which value should be the right part of a boolean expression
            and the name of the applied template constructor and applied functions
        - creates the reference to the variable inside of z. 
        - makes the referenced value to the value of the placeholder
-}
instance Lift BoolExpr where
    lift (BExpr x y z i j) = appE (appE (appE (appE (appE (conE 'BExpr) (lift x)) (lift y)) (lift z)) (appE (conE 'ValueA) (mkVarExtension x))) (appE (conE 'ValueA) (mkVarExtension z))
    lift (BExprS x y) = appE (appE (conE 'BExprS) (lift x)) (appE (conE 'ValueB) (mkVar x))

{-|
  multiplies a html element regarding to the specified iteration pattern. 
-}
mkFor:: For -> SingleValue -> ExpQ
mkFor (F x y z) s = case s of 
    (Single a) -> do
        let multSingle = compE [bindS (varP $ mkName x) (appE unpack_ (varE $ mkName y)), noBindS (lift a)]
        let single = appE (varE $ mkName "concat") (multSingle)
        appE (conE 'Single) (single)
        where 
            unpack_ = varE $ mkName "unpack"
mkFor (FM (x1:x2:[]) y z) s = case s of 
    (Single a) -> do
        let name = mkName x2
        let newList = compE [bindS (varP name) (mkVar y), noBindS (appE (conE $ mkName x1) (varE name))]

        let multSingle = compE [bindS (varP $ mkName x2) (appE unpack_ ((appE (conE 'T_List) newList))), noBindS (lift a)]
        let single = appE (varE $ mkName "concat") (multSingle)
        appE (conE 'Single) (single)
        where 
            unpack_ = varE $ mkName "unpack"
            map_ = varE $ mkName "map"

mkFor (N) s = case s of 
    (Single a) -> lift s

{-|
  The 'unpack' function unpacks the list of T_List constructor
-}
unpack :: Template -> [Template]
unpack a = case a of 
    T_List list -> list
    _ -> []

{-|
  The 'mkVar' function initiales a variable expression (template haskell code
-}
mkVar :: String -> ExpQ
mkVar a = varE $ mkName a

{-|
  The 'filterAttribute' function is used for filtering attributes which shouldn't be displayed in the final html.
-}
filterAttribute :: Attribute -> Bool
filterAttribute (A x) = True
filterAttribute (Av x y) = True
filterAttribute (If x) = False


{-|
  The 'mkVarExtension' function converts a placeholder pattern (template haskell code).
  e. g. T_String firstName user1
-}
mkVarExtension :: [String] -> ExpQ
mkVarExtension (c:[]) = mkVar c
mkVarExtension (c:list) = appE (conE $ mkName c) (myReverse list)
    where 
        myReverse (x1:[]) = mkVar x1
        myReverse (x1:xr) = appE (mkVar x1) (myReverse xr)


{-|
  The 'getElemName' function returns the name of a html element.
  e. g. div
-}
getElemName :: Element -> String
getElemName (EName a _)  = a

{-|
  The 'eval' function evaluates a combined set of boolean expression.  
  returns True or False.
-}
eval :: Element -> Bool
eval (EName a b) = all (\x -> x) (map (\x -> 
    case x of  
        If a -> evalSatis a
        _ -> True
    ) b)

{-|
  The 'evalSatis' evaluates a combined set of boolean expression.  
  returns True or False.
-}
evalSatis :: Expr -> Bool
evalSatis (Or e1 e2) = (evalSatis e1 || evalSatis e2)
evalSatis (And e1 e2) = (evalSatis e1 && evalSatis e2)
evalSatis (Var a) = evalBoolean a
evalSatis (SubExpr a) = evalSatis a
evalSatis (Not e) = not (evalSatis e)

{-|
  The 'evalBoolean' evaluates a single boolean expression.  
  returns True or False.
-}
evalBoolean :: BoolExpr -> Bool
evalBoolean (BExpr _ Eq _ x y) = x == y
evalBoolean (BExpr _ Lt _ x y) = x < y
evalBoolean (BExpr _ Gt _ x y) = x > y
evalBoolean (BExpr _ Le _ x y) = x <= y
evalBoolean (BExpr _ Ge _ x y) = x >= y
evalBoolean (BExpr _ Ne _ x y) = x /= y
evalBoolean _ = False

{-|
  applies show to a template haskell expression.  
-}
newShow :: ExpQ -> ExpQ
newShow x = appE (varE $ mkName "show") x

{-|
  creates a list comprehension in template haskell syntax for evaluating placeholder patterns.  
  placeholder patterns: e.g. T_String firstName user1
-}
mkList :: [String] -> String -> ExpQ
mkList (x1:x2:[]) y = compE [bindS (varP $ name) (varE $ mkName y), noBindS (appE (conE $ mkName x1) (varE name))]
    where name = mkName x2


