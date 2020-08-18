module Html where

import Text.ParserCombinators.Parsec hiding((<|>), many)
import Control.Applicative
import Control.Monad
import Helper

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