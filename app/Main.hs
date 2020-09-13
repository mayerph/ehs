{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-} 
{-# Language FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.ParserCombinators.Parsec hiding((<|>), many)
import Control.Applicative
import Control.Monad

import Html_Parser
import Html_Data
import Data
import Helper

main :: IO ()
main = do
    print "Embedded Haskell"


renderHtml :: String -> [HTMLValue] -> IO()
renderHtml file content = do
    writeFile file (list_to_string content)
