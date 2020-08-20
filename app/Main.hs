{-# LANGUAGE InstanceSigs #-}
module Main where

import Lib
import Parser
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
    print "hi"

--main :: IO ()
--main = do
--    x <- parseFromFile htmlContent "data.html"
--    case x of 
--        Right (a) -> do
--            print a
            

--main :: IO ()
--main = do
--    x <- parseFromFile openingtag "data.html"
--    case x of 
--        Right (OName a b) -> do
--            print a
--            print b

   
