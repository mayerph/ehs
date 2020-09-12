{-# LANGUAGE TemplateHaskell, QuasiQuotes #-} 
{-# Language FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where


import Ehs
import Data

main :: IO ()
main = do
    renderHtml "template/schedule.html" testHtmlDoc

a = T_String "wie gehts"

testHtmlDoc :: [HTMLValue]
testHtmlDoc = [html|<div>hi {{ a }} sonst so</div>|]
