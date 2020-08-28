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

type AttributeName = String

data Placeholder a = Null | ValueB Bool | ValueA a | P String
    deriving (Show, Eq, Ord)

data AttributeValue a = Value String | Placeholder String (Placeholder a)
    deriving(Show)

data Attribute a =  A AttributeName | Av AttributeName (AttributeValue a)
    deriving(Show)

-- data Tag = Name | Name' String [Attribute]

data Element a = EName String [Attribute a] 
    deriving(Show)

data ForWrapper a = FW (Element a) (For a)

data Content a = CText String | CVar String (Placeholder a)
    deriving(Show)

data SingleValue a = Single [(Element a, [HTMLValue a])]
    deriving(Show)


data HTMLValue a = HTML (For a) (SingleValue a)  | HContent (Content a)
    deriving(Show)

--                   a    wasser  ["Wassermelone, "Pfirsich"]
data For a = N | F String String [a] 
    deriving Show


instance Lift (For a) where
    lift (N) = (conE 'N)
    lift (F x y z) = appE (appE (appE (conE 'F) (lift x)) (lift y)) (mkVar y)

instance Lift (HTMLValue a) where
    lift (HTML x y) = appE (appE (conE 'HTML) (lift x)) (mkFor x y)
    --lift (HTML x y) = appE (appE (conE 'HTML) (lift x)) (lift y)
    lift (HContent i) = appE (conE 'HContent) (lift i)

instance Lift (SingleValue a) where
    lift (Single x) = appE (conE 'Single) (lift x)

instance Lift (Content a) where
    lift (CText i) = appE (conE 'CText) (lift i)
    lift (CVar x y) = appE (appE (conE 'CVar) (lift x)) (appE (conE 'ValueA) (mkVar x))

instance Lift (Element a) where
    lift (EName x y) = appE (appE (conE 'EName) (lift x)) (lift y)

instance Lift (Attribute a) where
    lift (A i) = appE (conE 'A) (lift i)
    lift (Av x y) = appE (appE (conE 'Av) (lift x)) (lift y)

instance Lift (AttributeValue a) where
    lift (Value i) = appE (conE 'Value) (lift i)
    lift (Placeholder x y) = appE (appE (conE 'Placeholder) (lift x)) (appE (conE 'P) (mkVar x))

instance Lift a => Lift (Placeholder a) where
    --lift (P i) = appE (conE 'P) (unboundVarE (mkName i))
    lift (P i) = appE (conE 'P) (lift i)
    lift (Null) = (conE 'Null)
    lift (ValueB i) = appE (conE 'ValueB) (lift i)
    lift (ValueA i) = appE (conE 'ValueA) (lift i)
    



--listOfAs (a:ax) = [a,a]
--mkFor (a:ax) = case (a) of
    --(Single x x') -> case x of 
        --(EName y y' y'') -> case y'' of
            --(N) -> [a]
            --(F y y' y'') -> [a, a]

--mkFor a (b:bs) = case a of 
    --(F x y z) -> [b]       
    --_ -> [b]  

--createList = $(listE [lift ("hey"::String), lift ("was"::String)])    
mkFor:: For a -> SingleValue a -> ExpQ
mkFor (F x y z) s = case s of 
    (Single a) -> do
        let multSingle = compE [bindS (varP $ mkName x) (varE $ mkName y), noBindS (appE (varE $ mkName "f") (lift a))]
        let single = appE (varE $ mkName "concat") (multSingle)
        appE (conE 'Single) (single)
mkFor (N) s = case s of 
    (Single a) -> lift s
    

         
        --(appE (appE (conE 'Single) (lift a)) (appE (appE (appE (conE 'F) (lift x)) (lift y)) (lift ([]::String))))
        

       
        



-- N | F String String [a] 


mkVar :: String -> ExpQ
mkVar a = varE $ mkName a

compile str = do 
    case parse htmlContent "" str of 
        Right (a) -> a
        Left (_) -> error "parse error"

html = QuasiQuoter {quoteExp  = lift . compile,
    quotePat  = error "no pats for html",
    quoteType  = error "no type for html",
    quoteDec  = error "no decs for html"
}

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
--content :: Parser String
--content = do
  --notFollowedBy openingtag
  --notFollowedBy closingtag
  --ws
  --text <- string "<" <|> many1 (noneOf "<\n{}")
  --rest <- content <|> pure ""
  --return (text ++ rest)
 
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
    char '['
    a <- some letter
    string "<-"
    b <- some letter
    char ']'
    return $ F a b []

  
-- | Parses a single attribute of html tag
-- Parses an attribute with values or without values
attribute :: Parser (Attribute a)
attribute = try attributeWithValue <|> attributeOnly

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


--teststuff:: IO ()
--teststuff = print [html|<html>Hello</html>|]