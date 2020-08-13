{-# LANGUAGE InstanceSigs #-}
module Main where

import Lib
import Text.ParserCombinators.Parsec hiding((<|>), many)
import Control.Applicative
import Control.Monad

-- parse jsonValue "test" "{\"wasser\" : \"melone\"}"

-- parse jsonBool "test" "false" --> is working --> Right (B False)
-- parse jsonBool "test" "falsessss" --> is working / but shouldn't --> Right (B False)
--          Solution --> parse (jsonBool <* eof) "test" "false" 
-- eof steht für end of file

main :: IO ()
main = do
    x <- parseFromFile jsonValue "data.txt"
    case x of 
        Right (O x') -> do
            print $ lookup "glossary" x'


   
