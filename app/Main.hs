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
    print "hi"



   
