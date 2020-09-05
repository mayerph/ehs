{-# LANGUAGE TemplateHaskell, QuasiQuotes #-} 
{-# Language FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where


import Ehs
import Data

main :: IO ()
main = print "Hello World"

a = T_String "wie gehts"

testHtmlDoc :: [HTMLValue]
testHtmlDoc = [html|<div>hi{{ a }} sonst so</div>|]
