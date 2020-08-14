module Helper where

import Text.ParserCombinators.Parsec hiding((<|>), many)
import Control.Applicative
import Control.Monad

-- many = applies the parser p zero or more times. Returns a list of the returned values of p.
-- oneOf = succeeds if the current character is in the supplied list of characters cs. Returns the parsed character.
ws :: Parser String
ws = many (oneOf " \t\n")
--ws = many (oneOf " \t\n")

-- gets a parser as parameter and 
-- first run the parser
-- after that: look for whitespaces and remove them
lexeme p = p <* ws