module Lib where

import Text.ParserCombinators.Parsec hiding((<|>), many)
import Control.Applicative
import Control.Monad
import Helper

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- *> = wenn matchTrue funktioniert hat, dann wende alwaysTrue Parser an
boolTrue :: Parser Bool
boolTrue = (string "true") *> (pure True)

boolFalse :: Parser Bool
boolFalse = (string "false") *> (pure False)

-- first try boolTrue. if it works give the result. if not run boolFalse. if it works give the result
bool :: Parser Bool
bool = boolTrue <|> boolFalse

-- char parser matches a single character
-- noneOf will matches a character as long at it isn't a quote
-- many: you give it a parser and it run it over and over again so long it can pull it anymore 
-- run these three in sequence and return the middle one, if it should works
-- find a quote --> find everything which isn't a quote --> find a quote
stringLiteral :: Parser String
stringLiteral = (lexeme $ char '"') *> (many (noneOf ['"'])) <* (lexeme $ char '"')


-- value = bool <|> stringLiteral
-- this does'nt work, because which type of parser? Parser Bool? Parser String?
-- solution: wrapper
data JSONValue = 
      B Bool
    | S String
    | A [JSONValue]
    | O [(String, JSONValue)]
    deriving(Show)

--- main parser
--- consinst of mulitple parsers
-- <?>  = custom error message
jsonValue :: Parser JSONValue
jsonValue =  jsonBool <|> (jsonStringLiterals <?> "string literal") <|> jsonObject <|> jsonArray
---

--(<||>) :: Parser a -> Parser a -> Parser a
--p <||> q = (try p) <|> q

jsonBool' :: Parser JSONValue
jsonBool' = B <$> bool

jsonBool :: Parser JSONValue
jsonBool = lexeme jsonBool'

jsonStringLiterals :: Parser JSONValue
jsonStringLiterals = lexeme (S <$> stringLiteral)




-- (jsonValue `sepBy` (char ','))
-- i want some jsonValue separated by komma
-- z. B ["Hallo", true, false, {"test" : "wasser"}]
array:: Parser [JSONValue]
array = (lexeme $ char '[') *> (jsonValue `sepBy` (lexeme $ char ',')) <* (lexeme $ char ']')

jsonArray:: Parser JSONValue
jsonArray = lexeme $ A <$> array



-- z. B "beer": true
objectEntry:: Parser (String, JSONValue)
objectEntry= do
    key <- stringLiteral
    lexeme $ char ':'
    value <- jsonValue
    return (key, value)

-- z. B { "beer": true }
jsonObject :: Parser JSONValue
jsonObject = lexeme $ O <$> ((lexeme $ char '{') *> (objectEntry `sepBy` (lexeme $ char ',')) <* (lexeme $ char '}'))


