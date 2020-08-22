
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

data Simple2 a = S2 String | V2 String | Y2 String String [a]
    deriving Show


data MyText2 a = T2 [Simple2 a] 
    deriving Show   



instance Lift (Simple2 a) where
    lift (S2 i) = appE (conE 'S2) (lift i)
    lift (V2 i) = appE (conE 'V2) (unboundVarE (mkName i))
    lift (Y2 x y z) = appE (appE (appE (conE 'Y2) (lift x)) (lift y)) (listOfAs' y)

instance Lift (MyText2 a) where
    lift (T2 i) = appE (conE 'T2) (lift i)



compile str = do 
    case parse parseSimple "" str of 
        Right (a) -> a
        Left (_) -> error "parse error"

simple = QuasiQuoter {quoteExp  = lift . compile,
    quotePat  = error "no pats for simple",
    quoteType  = error "no type for simple",
    quoteDec  = error "no decs for simple"
}

listOfAs :: ExpQ
listOfAs = (listE (map varE [ mkName ('a' : show n) | n <- [1..2] ]))

listOfAs' :: String -> ExpQ
listOfAs' a = varE $ mkName a

parseSimple :: Parser (MyText2 a)
parseSimple = T2 <$> (some (parseText <|> parseVar <|> parseList))

parseText :: Parser (Simple2 a)
parseText = S2 <$> (some (letter <|> (oneOf " ")))


parseVar :: Parser (Simple2 a)
parseVar = do
    char '{'
    val <- some letter
    char '}'
    return $ V2 val


parseList :: Parser (Simple2 a)
parseList = do
    char '['
    a <- some letter
    string "<-"
    b <- some letter
    char ']'
    return $ Y2 a b []