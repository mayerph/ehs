module Parser where

import Text.ParserCombinators.Parsec hiding((<|>), many)
import Control.Applicative
import Control.Monad
import Helper

type AttributeName = String
type AttributeValue = String

data Attribute =  A AttributeName [AttributeValue] | A' AttributeName AttributeValue
    deriving(Show)
-- data Tag = Name | Name' String [Attribute]
data Element = Name String [Attribute]
    deriving(Show)

data HTMLValue = HTML Element [HTMLValue] | Content String
    deriving(Show)

htmlParser :: Parser HTMLValue
htmlParser = do
    element <- openingtag 
    val <-  htmlContent
    closingName <- closingtag
    let elementName = case element of Name a b -> a
    case elementName == closingName of 
        False -> fail "my failure"
        True -> return $ HTML element val

-- html structure or content
-- parses the content of a html document
htmlContent :: Parser [HTMLValue]
htmlContent = some $ (try htmlParser) <|> (Content <$> (ws *> ((some $ noneOf "<,\n")) <* ws))
--htmlContent = many $ (try htmlParser) <|> (Content <$> (ws *> (try satisfy' <|> (some (noneOf "</"))) <* ws))


-- | Parses the closing tag of a html element
-- e.g. </div>
closingtag :: Parser String
closingtag = ws *> char '<' *> char '/' *> (some letter) <* ws <* char '>' <* ws

-- | Parses the opening tag of a html element
-- e.g. <div id="parent">
openingtag :: Parser Element
openingtag = do
    tagName <- ws *> char '<' *> some letter <* ws
    attr <- many attribute
    char '>'
    return $ Name tagName attr

  
-- | Parses a single attribute of html tag
-- Parses an attribute with values or without values
attribute :: Parser Attribute
attribute = try attributeWithValue <|> attributeOnly

-- | Parses an attribute with values
-- e.g. <div class="wrapper"></div>
-- class="wrapper"
attributeWithValue:: Parser Attribute
attributeWithValue= do
    attr <- ws *> some letter <* ws
    char '=' <* ws
    char '"' <* ws
    value <- many ((noneOf "\"\t\n") <* many (oneOf "\"\t\n"))
    char '"' <* ws
    return $ A' attr value

-- | Parses an attribute without values
-- e.g. <div hidden></div>
-- hidden
attributeOnly :: Parser Attribute
attributeOnly = do
    ws
    name <- some letter
    ws 
    return $ A name []

attributeValue :: Parser String
attributeValue = do
    value <- ws *> many ((noneOf " \"\t\n") <* many (oneOf " \"\t\n"))
    return value

helpers :: Parser String
helpers = do
    val <- ws *> removeChars "\t\n" 
    return val
-- es gibt nur ein value
-- dennoch suchen wir nach \n und blanks und trennen die values
-- damit wir sie später wieder zusammensetzen können





attributeValue' :: Parser String
attributeValue' = do
    val <- char 'n' *> many (noneOf "")
    return val