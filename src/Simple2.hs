
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-} 
{-# Language FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Simple2(simple2, MyData, MySimple) where
import Data.List
import Language.Haskell.TH 
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax 
import Control.Monad

import Text.ParserCombinators.Parsec hiding((<|>), many)
import Control.Applicative
import Helper


--                                            a    wasser  ["Wassermelone, "Pfirsich"]

data MyData a = No | Something a
    deriving Show
    
data MySimple a = Normal String (MyData a)
    deriving Show

data AntiQuote = AQ String
    deriving Show

instance Lift (MySimple a) where
    lift (Normal x y) = appE (appE (conE 'Normal) (lift x)) (appE (conE 'Something) (mkVar x))

instance Lift a => Lift (MyData a) where
    lift (No) = conE 'No
    lift (Something i) = appE (conE 'Something) (lift i)

instance Lift (AntiQuote) where
    lift (AQ i) = appE (conE 'AQ) (mkVar i)



compile str = do 
    case parse parseAntiQuote "" str of 
        Right (a) -> a
        Left (_) -> error "parse error"

simple2 = QuasiQuoter {quoteExp  = lift . compile,
    quotePat  = error "no pats for simple",
    quoteType  = error "no type for simple",
    quoteDec  = error "no decs for simple"
}

mkVar :: String -> ExpQ
mkVar a = varE $ mkName a

parseVar :: Parser (MySimple a)
parseVar = do
    ws
    char '{' 
    ws
    val <- some letter
    ws
    char '}'
    ws
    return $ Normal val No

parseAntiQuote :: Parser AntiQuote
parseAntiQuote = do
    char '$'
    val <- some letter
    return $ AQ val

