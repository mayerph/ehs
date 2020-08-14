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
data OpeningTag = ON String [Attribute]
    deriving(Show)

data ClosingTag = CN String
    deriving(Show)

data HTML = H OpeningTag [HTML] ClosingTag | C String

htmlParser :: Parser String
htmlParser = do 
    oTag <- openingtag 
    val <- many letter 
    cTag <- closingtag
    let oTagName = case oTag of ON a b -> a
    let cTagName = case cTag of CN a -> a
    case oTagName == cTagName of 
        False -> fail "my failure"
        True -> return val
    

closingtag :: Parser ClosingTag
closingtag = char '<' *> char '/' *> (CN <$> some letter) <* spaces <* char '>'

openingtag :: Parser OpeningTag
openingtag = do
    tagName <- char '<' *> some letter
    attr <- many attribute
    char '>'
    return $ ON tagName attr

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

nameParser:: Parser String
nameParser = undefined


--openingTag :: Parser JSONValue
--openingTag = lexeme $ O <$> ((lexeme $ char '{') *> (objectEntry `sepBy` (lexeme $ char ',')) <* (lexeme $ char '}'))