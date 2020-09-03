
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Html where
import Data.List
import Language.Haskell.TH 
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax 
import Control.Monad
import Text.ParserCombinators.Parsec hiding((<|>), many)
import Control.Applicative

import Helper

data Sim a = P String String [a] 
    deriving Show

data Cont = Co String | 
data Html = Element String [Html] | Content String
    deriving Show

element :: Parser Html
element = do
    name <- char '<' *> many1 letter <* char '>'
    children <- many $ (try element) <|> content
    string "</" >> string name >> char '>'
    return $ Element name children

content :: Parser Html
content = fmap Content $ many1 $ satisfy (\x -> x /='<')