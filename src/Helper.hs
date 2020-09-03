{-# Language FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Helper where

import Text.ParserCombinators.Parsec hiding((<|>), many)
import Control.Applicative
import Control.Monad


ws :: Parser String
ws = many (oneOf " \t\n")

lexeme p = p <* ws

removeChars:: String -> Parser String
removeChars a = many ((noneOf a) <* many (oneOf a))

list_to_string :: Show a => [a] -> String
list_to_string = unwords . map show