{-# Language FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Helper where

import Text.ParserCombinators.Parsec hiding((<|>), many)
import Control.Applicative
import Control.Monad

{-|
  the 'ws' function reads new lines, tabs and whitespaces
-}
ws :: Parser String
ws = many (oneOf " \t\n")


{-|
  The 'list_to_string' function converts a list to a string. 
-}
list_to_string :: Show a => [a] -> String
list_to_string = unwords . map show
