{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Parser where

import Data.List
import Language.Haskell.TH 
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax 
import Control.Monad

import Text.ParserCombinators.Parsec hiding((<|>), many)
import Control.Applicative
import Helper
    




type AttributeName = String
data Operator = Eq | Lt | Gt | Le | Ge | Ne

data Placeholder = P String
    deriving(Show)
data AttributeValue = Value String | Placeholder Placeholder
    deriving(Show)
data Attribute =  A AttributeName | Av AttributeName AttributeValue 
    deriving(Show)
-- data Tag = Name | Name' String [Attribute]
data Element = EName String [Attribute]
    deriving(Show)

data HTMLValue = HTML Element [HTMLValue] | Content String
    deriving(Show)


instance Lift HTMLValue where
    lift (HTML x y) = appE (appE (conE 'HTML) (lift x)) (lift y)
    lift (Content i) = appE (conE 'Content) (unboundVarE (mkName i))

instance Lift Element where
    lift (EName x y) = appE (appE (conE 'EName) (lift x)) (lift y)

instance Lift Attribute where
    lift (A i) = appE (conE 'A) (lift i)
    lift (Av x y) = appE (appE (conE 'Av) (lift x)) (lift y)

instance Lift AttributeValue where
    lift (Value i) = appE (conE 'Value) (lift i)
    lift (Placeholder i) = appE (conE 'Placeholder) (lift i)

instance Lift Placeholder where
    lift (P i) = appE (conE 'P) (unboundVarE (mkName i))
    

compile str = do 
    case parse htmlContent "" str of 
        Right (a) -> a
        Left (_) -> error "parse error"

html = QuasiQuoter {quoteExp  = lift . compile,
    quotePat  = error "no pats for html",
    quoteType  = error "no type for html",
    quoteDec  = error "no decs for html"
}

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
htmlContent = many $ (try htmlParser) <|> (Content <$> content <* ws)


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
    return $ Av attr value

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