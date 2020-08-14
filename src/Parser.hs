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
data OpeningTag = OName String [Attribute]
    deriving(Show)

data ClosingTag = CName String
    deriving(Show)

data HTMLValue = HTML OpeningTag [HTMLValue] ClosingTag | Content String
    deriving(Show)

htmlParser :: Parser HTMLValue
htmlParser = do
    oTag <- openingtag 
    val <- many htmlContent
    cTag <- closingtag
    let oTagName = case oTag of OName a b -> a
    let cTagName = case cTag of CName a -> a
    case oTagName == cTagName of 
        False -> fail "my failure"
        True -> return $ HTML oTag val cTag

-- html structure or content
htmlContent :: Parser HTMLValue
htmlContent = (try htmlParser) <|> (Content <$> (ws *> some letter <* ws))

closingtag :: Parser ClosingTag
closingtag = ws *> char '<' *> char '/' *> (CName <$> some letter) <* ws <* char '>' <* ws

openingtag :: Parser OpeningTag
openingtag = do
    tagName <- ws *> char '<' *> some letter <* ws
    attr <- many attribute
    char '>'
    return $ OName tagName attr

-- parentAttribute
-- parentAttribute = attribute <|> attribute':
attribute :: Parser Attribute
attribute = try attributeValue <|> attributeOnly


attributeValue:: Parser Attribute
attributeValue= do
    attr <- ws *> some letter <* ws
    char '=' <* ws
    char '"' <* ws
    value <- (many (noneOf ['"'])) <*ws
    char '"' <* ws
    return $ A' attr value

attributeOnly :: Parser Attribute
attributeOnly = A <$> (ws *> spaces *> some letter <* spaces <* ws) 

nameParser:: Parser String
nameParser = undefined


--openingTag :: Parser JSONValue
--openingTag = lexeme $ O <$> ((lexeme $ char '{') *> (objectEntry `sepBy` (lexeme $ char ',')) <* (lexeme $ char '}'))