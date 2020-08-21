{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module TestParser where
import Parser

test = "Wassermelone"
wasser = "fan"
myId = "was ist los"

testParser = print [html|<div class="test" id="{{myId}}"></div>|]