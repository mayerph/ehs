module Parser where

import Text.ParserCombinators.Parsec hiding((<|>), many)
import Control.Applicative
import Control.Monad
import Helper

doIt :: IO ()
doIt = putStrLn "doIt"







closingTag :: Parser String
closingTag = (lexeme $ string "</") *> (many (noneOf ['<', '>'])) <* (lexeme $ char '>')



type AttributeName = String
type AttributeValue = String

data Attribute =  A AttributeName | A' AttributeName AttributeValue
    deriving(Show)
-- data Tag = Name | Name' String [Attribute]
data OpeningTag = Name String | Name' String [Attribute]
    deriving(Show)


openingtag :: Parser OpeningTag
openingtag = do
    tagName <- tagnameParser' 
    attr <- many attribute
    return $ Name' tagName attr

-- parentAttribute
-- parentAttribute = attribute <|> attribute':
attribute :: Parser Attribute
attribute = try attributeValue <|> attributeOnly


attributeValue:: Parser Attribute
attributeValue= do
    spaces
    attr <- some letter
    spaces
    char '='
    spaces
    char '"'
    value <- (many (noneOf ['"'])) 
    char '"'
    return $ A' attr value

attributeOnly :: Parser Attribute
attributeOnly = A <$> (spaces *> some letter <* spaces)

tagnameParser :: Parser OpeningTag
tagnameParser = Name <$> tagnameParser'


tagnameParser' :: Parser String
tagnameParser' = char '<' *> some letter

nameParser:: Parser String
nameParser = undefined


--openingTag :: Parser JSONValue
--openingTag = lexeme $ O <$> ((lexeme $ char '{') *> (objectEntry `sepBy` (lexeme $ char ',')) <* (lexeme $ char '}'))