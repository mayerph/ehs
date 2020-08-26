
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


--                                            a    wasser  ["Wassermelone, "Pfirsich"]
data Simple2 a = S2 String [a] | V2 String | Y2 String String [a] | If String Bool
    deriving Show


data MyText2 a = T2 [Simple2 a] 
    deriving Show   



instance Lift (Simple2 a) where
    lift (S2 x y) = appE (appE (conE 'S2) (lift x)) (lift createList)
    lift (V2 x) = appE (conE 'V2) (lift x)
    lift (Y2 x y z) = appE (appE (appE (conE 'Y2) (lift x)) (lift y)) (listOfAs' y)
    lift (If x y) = appE (appE (conE 'If) (unboundVarE (mkName x))) (lift y)

instance Lift (MyText2 a) where
    lift (T2 i) = appE (conE 'T2) (lift i)


createList = $(listE [lift ("hey"::String), lift ("was"::String)])
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
parseSimple = T2 <$> (some ((try parseText) <|> (try parseVar) <|> (try parseList) <|> (try parseBool)))

parseText :: Parser (Simple2 a)
parseText = do 
    val <- (some (letter <|> (oneOf " ")))
    return $ S2 val []

parseVar :: Parser (Simple2 a)
parseVar = do
    char '{'
    val <- some letter
    char '}'
    return $ V2 val

parseBool :: Parser (Simple2 a)
parseBool = do
    char '<'
    val <- some letter
    char '>'
    return $ If val False


parseList :: Parser (Simple2 a)
parseList = do
    char '['
    a <- some letter
    string "<-"
    b <- some letter
    char ']'
    return $ Y2 a b []