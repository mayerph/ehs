{-# LANGUAGE TemplateHaskell, QuasiQuotes #-} 
{-# Language FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Simple where
import Data.List
import Language.Haskell.TH 
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax 
import Control.Monad

import Text.ParserCombinators.Parsec hiding((<|>), many)
import Control.Applicative
import Helper

data PlaceholderSimple = NullS | PSimple Template
    deriving (Eq, Ord, Show)

data SomeValue = PlaceholderS String PlaceholderSimple | PlaceholderM [String] PlaceholderSimple
    deriving (Show)

instance Lift PlaceholderSimple where
    lift (PSimple i) = appE (conE 'PSimple) (lift i)
    lift (NullS) = (conE 'NullS)

instance Lift SomeValue where  
   
    lift (PlaceholderM x y) = appE (appE (conE 'PlaceholderM) (lift x)) (appE (conE 'PSimple) (mkVarExt x))


compile str = do 
    case parse checkVars "" str of 
        Right (a) -> a
        Left (_) -> error "parse error"

simple = QuasiQuoter {quoteExp  = lift . compile,
    quotePat  = error "no pats for simple",
    quoteType  = error "no type for simple",
    quoteDec  = error "no decs for simple"
}

mkVar :: String -> ExpQ
mkVar a = varE $ mkName a

mkVarExt :: [String] -> ExpQ
mkVarExt (x1:x2:x3:xr) = appE (conE $ mkName x1) (appE (mkVar x2) (mkVar x3))
--mkVarExt (x1:[]) = varE $ mkName x1

checkVars :: Parser SomeValue
checkVars = checkMulti

checkMulti :: Parser SomeValue
checkMulti = do
    val <- some (some (noneOf " ") <* ws)
    return $ PlaceholderM val NullS

-- Haskell

data Template = 
        Empty
    |   T_List [Template]
    |   T_String String 
    |   T_Int Int 

    deriving (Eq, Ord, Show)

instance Lift Template where
    lift (T_String i) = appE (conE 'T_String) (lift i)
    lift (T_Int i) = appE (conE 'T_Int) (lift i)
