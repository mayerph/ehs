module Parser where

import Text.ParserCombinators.Parsec hiding((<|>), many)
import Control.Applicative
import Control.Monad
import Helper
import Language.Haskell.TH 
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax 



type AttributeName = String
data Operator = Eq | Lt | Gt | Le | Ge | Ne

data Placeholder = P String
    deriving(Show)
data AttributeValue = Value String | Placeholder Placeholder
    deriving(Show)
data Attribute =  A AttributeName | A' AttributeName AttributeValue 
    deriving(Show)
-- data Tag = Name | Name' String [Attribute]
data Element = EName String [Attribute]
    deriving(Show)

data HTMLValue = HTML Element [HTMLValue] | Content String
    deriving(Show)

htmlParser :: Parser HTMLValue
htmlParser = do
    element <- openingtag 
    val <-  htmlContent
    closingName <- closingtag
    let elementName = case element of EName a b -> a
    case elementName == closingName of 
        False -> fail "my failure"
        True -> return $ HTML element val

-- html structure or content
-- parses the content of a html document
htmlContent :: Parser [HTMLValue]
htmlContent = some $ (try htmlParser) <|> (Content <$> content <* ws)


content :: Parser String
content = do
  notFollowedBy openingtag
  notFollowedBy closingtag
  ws
  text <- string "<" <|> many1 (noneOf "<\n")
  rest <- content <|> pure ""
  return (text ++ rest)

directive :: Parser String
directive = do
    string "*h" 
    d <- (string "If" <|> string "For")
    ws
    char '='
    ws
    char '"'
    val <- many (letter)
    return $ d ++ val

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
    return $ EName tagName attr



  
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
    --value <- (many (noneOf ['"'])) <*ws
    value <- (try attributePlaceholder <|> attributeValue) <* ws
    char '"' <* ws
    return $ A' attr value

attributeValue :: Parser AttributeValue
attributeValue = do
    value <-  ws *> many ((noneOf "\"\t\n{}") <* many (oneOf "\t\n")) 
    return $ Value value

attributePlaceholder :: Parser AttributeValue
attributePlaceholder = Placeholder <$> placeholder


placeholder :: Parser Placeholder
placeholder = do
    char '{' *> ws *> char '{'
    value <- ws *> some (letter <|> oneOf "-_" <|> digit) <* ws
    char '}' *> ws *> char '}'
    return $ P value


-- | Parses an attribute without values
-- e.g. <div hidden></div>
-- hidden
attributeOnly :: Parser Attribute
attributeOnly = do
    ws
    name <- some letter
    ws 
    return $ A name


--teststuff:: IO ()
--teststuff = print [html|<html>Hello</html>|]