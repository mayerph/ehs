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


{-|
  Defines the QuasiQuoter for the templating engine.
  It calls the root-parser and converts it values into valid html code. 
-}
html = QuasiQuoter {quoteExp  = lift . compile,
    quotePat  = error "no pats for html",
    quoteType  = error "no type for html",
    quoteDec  = error "no decs for html"
}

{-|
  The 'compile' function parses a html string and returns a valid haskell data type. 
-}
compile str = do 
    case parse htmlContent "" str of 
        Right (a) -> a
        Left (_) -> error "parse error"

{-|
  The 'htmlContent' function is the root parser for the html templating engine. 
  It parses all elements, content and placeholders of a html document. 
-}
htmlContent :: Parser [HTMLValue]
htmlContent = many $ (try htmlParser) <|> (HContent <$> (try (contentPlaceholder) <|> (CText <$> content <* ws)))

{-|
  The 'htmlParser' function parses all elements and content a single html element. 
-}
htmlParser :: Parser HTMLValue
htmlParser = do
    forWr <- openingtag 
    let element = case forWr of (FW a b) -> a
    let for = case forWr of (FW a b) -> b
    val <-  htmlContent
    closingName <- closingtag
    let elementName = case element of (EName a b) -> a

    case elementName == closingName of 
        False -> fail "my failure"
        True -> return $ HTML for (Single [(element, val)])

{-|
  The 'contentPlaceholder' function parses a placeholder pattern defined in the content section of a html element. 
-}
contentPlaceholder :: Parser Content
contentPlaceholder = do
    p <- placeholderM
    return $ CVarM p Null

{-|
  The 'content' function parses the static content of a html element. 
  e. g. Hello World and happy coding
-}
content :: Parser String
content = (ws *> ((some $ noneOf "<\n{}")) <* ws)

{-|
  The 'openingPlaceholder' function parses the startpoint of a placeholder pattern for the content section. 
-}
openingPlaceholder :: Parser Char
openingPlaceholder = char '{' *> ws *> char '{'

{-|
  The 'closingPlaceholder' function parses the endpoint of a placeholder pattern for the content section. 
-}
closingPlaceholder :: Parser Char
closingPlaceholder = char '}' *> ws *> char '}'

{-| 
    The 'closingtag' function parses the closing tag of a html element
    e.g. </div>
-}
closingtag :: Parser String
closingtag = ws *> char '<' *> char '/' *> (some letter) <* ws <* char '>' <* ws

{-| 
    The 'openingtag' function parses the opening tag of a html element
    e.g. <div [user <- users] id="parent">
-}
openingtag :: Parser ForWrapper
openingtag = do
    tagName <- ws *> char '<' *> some letter <* ws
    for <- many parseList
    attr <- many attribute
    char '>'
    case for of
        (a:[]) -> return $ FW (EName tagName attr) a
        ([]) -> return $ FW (EName tagName attr) N
        _ -> fail "Multiple for declarations"

{-| 
    The 'parseList' function parses the iteration pattern for a html element.
    e.g. [user <- users]
-}
parseList :: Parser For
parseList = do
    char '[' <* ws
    a <- some letter <* ws
    string "<-" <* ws
    b <- some letter <* ws
    char ']'
    return $ F a b Empty

{-| 
    The 'parseListM' function parses the iteration pattern for a html element.
    e.g. [user <- users]
    For the moment not in use. 
-}
parseListM :: Parser For
parseListM = do
    char '[' <* ws
    a <- some (some (noneOf "<- ") <* ws)
    string "<-" <* ws
    b <- some letter <* ws
    char ']'
    return $ FM a b Empty

{-| 
    The 'attribute' function parses a single attribute of html tag.
    There are three possibilities what an attribute can be:
        1. an boolean expression pattern (hIf)
        2. an attribute with values
        3. an attribute without values
    Parses an attribute with values or without values
-}
attribute :: Parser Attribute
attribute = (try hIf) <|> attributeWithValue <|> attributeOnly

{-| 
    The 'hIf' function parses a boolean expression pattern
    e.g. hIf="T_String role1 != T_String role2 'AND' ('NOT' T_Int a > T_Int b)"
-}
hIf :: Parser Attribute
hIf = do
    string "hIf"
    char '='
    char '"'
    val <- satisfiability
    char '"'
    return $ If val

{-| 
    The 'attributeWithValue' function parses an attribute with values.
    The value can be static or a placeholder.
    e.g. class="wrapper"
    e.g. class="{ T_String a }"
-}
attributeWithValue:: Parser Attribute
attributeWithValue= do
    attr <- ws *> some letter <* ws
    char '=' <* ws
    char '"' <* ws
    value <- (try attributePlaceholder <|> attributeValue) <* ws
    char '"' <* ws
    return $ Av attr value

{-| 
    The 'attributeValue' function parses the static value of an attribute.
-}
attributeValue :: Parser AttributeValue
attributeValue = do
    value <-  ws *> many ((noneOf "\"\t\n{}") <* many (oneOf "\t\n")) 
    return $ Value value

{-| 
    The 'attributePlaceholder' function parses the placeholder pattern of of an attribute value.
    It converts the value from String to AttributeValue
-}
attributePlaceholder :: Parser AttributeValue
attributePlaceholder = do 
    p <- placeholderM
    return $ PlaceholderM p Null

{-| 
    The 'placeholderM' function parses the placeholder pattern of of an attribute value.
    e.g '{ T_String a }'
-}
placeholderM :: Parser [String]
placeholderM = do
    char '{'
    ws
    value <- some (some ((try letter) <|> (try (oneOf "-_")) <|> try digit) <* ws)
    ws
    char '}'
    return value
    

{-| 
    The 'attributeOnly' function parses an attribute without values.
    e.g hidden
-}
attributeOnly :: Parser Attribute
attributeOnly = do
    ws
    name <- some letter
    ws 
    return $ A name

{-| 
    The 'satisfiability' function parses an boolean expression pattern.
    e.g T_String role1 != T_String role2 'AND' ('NOT' T_Int a > T_Int b)
-}
satisfiability :: Parser Expr
satisfiability = expr 
    where   expr = buildExpressionParser operators term <?> "compound expression"
            term      =  parens expr <|> variable <?> "full expression"
            operators = [ [Prefix (string "'NOT'" >> ws >> return Not)]
                      , [binary "'AND'" And]
                      , [binary "'OR'" Or] ]
                where binary n c = ParsecExpr.Infix (string n *> ws *> pure c) AssocLeft
            parens p = SubExpr <$> (char '(' *> ws *> p <* char ')' <* ws) <?> "parens"

{-| 
    The 'varialbe' function parses a placeholder.
-}
variable :: Parser Expr
variable = do 
    a <- boolExpr
    return $ Var a

{-| 
    The 'boolExpr' function parses a single boolean expression pattern.
    It can be written down expression or a placeholder for an expression 
-}
boolExpr :: Parser BoolExpr
boolExpr = (try boolExprM) <|> (try boolExprS)

{-| 
    The 'boolExprM' function parses a single boolean expression pattern.
    e.g T_String role1 != T_String role2
-}
boolExprM :: Parser BoolExpr
boolExprM = do
    var1 <- some (some ((try letter) <|> (try (oneOf "-_")) <|> try digit) <* ws)
    ws
    op <- boolOp 
    ws
    var2 <- some (some ((try letter) <|> (try (oneOf "-_")) <|> try digit) <* ws)
    ws
    return $ BExpr var1 op var2 Null Null

{-| 
    The 'boolExprS' function parses a placeholder pattern which evalutes to a boolean expression.
-}
boolExprS :: Parser BoolExpr
boolExprS = do 
    a <- ((some (letter)) <* ws) <?> "variable"
    return $ BExprS a Null

{-| 
    The 'boolOp' function parses the boolean operators.
    e.g >=
-}
boolOp :: Parser BoolOp
boolOp = string "==" *> pure Eq 
    <|>  string "<" *> pure Lt
    <|>  string ">" *> pure Gt
    <|>  string "<=" *> pure Le
    <|>  string ">=" *> pure Ge
    <|>  string "!=" *> pure Ne