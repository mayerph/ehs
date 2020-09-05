{-# LANGUAGE TemplateHaskell, QuasiQuotes #-} 
{-# Language FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where


import Ehs

main :: IO ()
main = print "Hello World"

testHtmlDoc :: [HTMLValue]
testHtmlDoc = [html|<div>hi</div>|]
