{-# LANGUAGE TemplateHaskell, QuasiQuotes #-} 
{-# Language FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Html_Parser where

import Data.List
import Language.Haskell.TH 
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax 
import Control.Monad
import Control.Applicative

import Text.ParserCombinators.Parsec hiding((<|>), many)
import Text.ParserCombinators.Parsec.Token hiding (parens)
import Text.ParserCombinators.Parsec.Expr as ParsecExpr


import Helper
import Data
import Html_Data

compile str = do 
    case parse htmlContent "" str of 
        Right (a) -> a
        Left (_) -> error "parse error"

html = QuasiQuoter {quoteExp  = lift . compile,
    quotePat  = error "no pats for html",
    quoteType  = error "no type for html",
    quoteDec  = error "no decs for html"
}

myStringParser :: Parser String
myStringParser = some letter

htmlParser :: Parser (HTMLValue a)
htmlParser = do
    -- wir holen die For Information aus dem opening-tag indem wir einen neuen Datentypen einführen
    forWr <- openingtag 
    let element = case forWr of (FW a b) -> a
    let for = case forWr of (FW a b) -> b
    val <-  htmlContent
    closingName <- closingtag
    let elementName = case element of (EName a b) -> a

    case elementName == closingName of 
        False -> fail "my failure"
        True -> return $ HTML for (Single [(element, val)])

-- html structure or content
-- parses the content of a html document
htmlContent :: Parser [(HTMLValue a)]
htmlContent = many $ (try htmlParser) <|> (HContent <$> (try (contentPlaceholder) <|> (CText <$> content <* ws)))

contentPlaceholder :: Parser (Content a)
contentPlaceholder = do
    p <- placeholder
    return $ CVar p Null
 
content :: Parser String
content = (ws *> ((some $ noneOf "<,\n{}")) <* ws)


openingPlaceholder :: Parser Char
openingPlaceholder = char '{' *> ws *> char '{'

closingPlaceholder :: Parser Char
closingPlaceholder = char '}' *> ws *> char '}'

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
openingtag :: Parser (ForWrapper a)
openingtag = do
    tagName <- ws *> char '<' *> some letter <* ws
    for <- many parseList
    attr <- many attribute
    char '>'
    case for of
        (a:[]) -> return $ FW (EName tagName attr) a
        ([]) -> return $ FW (EName tagName attr) N
        _ -> fail "Multiple for declarations"
    
parseList :: Parser (For a)
parseList = do
    char '[' <* ws
    a <- some letter <* ws
    string "<-" <* ws
    b <- some letter <* ws
    char ']'
    return $ F a b Empty

  
-- | Parses a single attribute of html tag
-- Parses an attribute with values or without values
attribute :: Parser (Attribute a)
--attribute = try hIf <|> attributeWithValue <|> attributeOnly
attribute = (try hIf) <|> attributeWithValue <|> attributeOnly

hIf :: Parser (Attribute a)
hIf = do
    string "hIf"
    char '='
    char '"'
    val <- satisfiability
    char '"'
    return $ If val
-- | Parses an attribute with values
-- e.g. <div class="wrapper"></div>
-- class="wrapper"
attributeWithValue:: Parser (Attribute a)
attributeWithValue= do
    attr <- ws *> some letter <* ws
    char '=' <* ws
    char '"' <* ws
    --value <- (many (noneOf ['"'])) <*ws
    value <- (try attributePlaceholder <|> attributeValue) <* ws
    char '"' <* ws
    return $ Av attr value

attributeValue :: Parser (AttributeValue a)
attributeValue = do
    value <-  ws *> many ((noneOf "\"\t\n{}") <* many (oneOf "\t\n")) 
    return $ Value value

attributePlaceholder :: Parser (AttributeValue a)
attributePlaceholder = do 
    p <- placeholder'
    return $ Placeholder p Null

placeholder :: Parser String
placeholder = do
    openingPlaceholder
    value <- ws *> some (letter <|> oneOf "-_" <|> digit) <* ws
    closingPlaceholder
    return value

placeholder' :: Parser String
placeholder' = do
    char '{'
    value <- ws *> some (letter <|> oneOf "-_" <|> digit) <* ws
    char '}'
    return value
    


-- | Parses an attribute without values
-- e.g. <div hidden></div>
-- hidden
attributeOnly :: Parser (Attribute a)
attributeOnly = do
    ws
    name <- some letter
    ws 
    return $ A name

satisfiability :: Parser (Expr a)
satisfiability = expr 
    where   expr = buildExpressionParser operators term <?> "compound expression"
            term      =  parens expr <|> variable <?> "full expression"
            operators = [ [Prefix (string "NOT" >> ws >> return Not)]
                      , [binary "AND" And]
                      , [binary "OR" Or] ]
                where binary n c = ParsecExpr.Infix (string n *> ws *> pure c) AssocLeft
            parens p = SubExpr <$> (char '(' *> ws *> p <* char ')' <* ws) <?> "parens"

-- testSatis = [sat|d AND e|]
variable :: Parser (Expr a)
variable = do 
    a <- boolExpr
    return $ Var a

boolExpr :: Parser (BoolExpr a)
boolExpr = (try boolExprM) <|> (try boolExprS)

boolExprM :: Parser (BoolExpr a)
boolExprM = do
    --notFollowedBy opString
    var1 <- some (letter <|> digit)
    ws
    op <- boolOp 
    ws
    var2 <- some (letter <|> digit)
    ws
    return $ BExpr var1 op var2 Null Null

boolExprS :: Parser (BoolExpr a)
boolExprS = do 
    a <- ((some (letter)) <* ws) <?> "variable"
    return $ BExprS a Null


boolOp :: Parser BoolOp
boolOp = string "==" *> pure Eq 
    <|>  string "<" *> pure Lt
    <|>  string ">" *> pure Gt
    <|>  string "<=" *> pure Le
    <|>  string ">=" *> pure Ge
    <|>  string "!=" *> pure Ne
    

