
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Simple where
import Data.List
import Language.Haskell.TH 
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax 
import Control.Monad

import Text.ParserCombinators.Parsec hiding((<|>), many)
import Control.Applicative
import Helper

data Simple = S String | V String
    deriving Show
data MyText = T [Simple]
    deriving Show


instance Lift Simple where
    lift (S i) = appE (conE 'S) (lift i)
    lift (V i) = appE (conE 'V) (unboundVarE (mkName i))

instance Lift MyText where
    lift (T i) = appE (conE 'T) (lift i)

compile str = do 
    case parse parseSimple "" str of 
        Right (a) -> a
        Left (_) -> error "parse error"

simple = QuasiQuoter {quoteExp  = lift . compile,
    quotePat  = error "no pats for simple",
    quoteType  = error "no type for simple",
    quoteDec  = error "no decs for simple"
}



parseSimple :: Parser MyText
parseSimple = T <$> (some (parseText <|> parseVar))

parseText :: Parser Simple
parseText = S <$> (some (letter <|> (oneOf " ")))


parseVar :: Parser Simple
parseVar = do
    char '{'
    val <- some letter
    char '}'
    return $ V val