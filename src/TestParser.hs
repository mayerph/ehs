{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module TestParser where
import Parser

test = "Wassermelone"
wasser = "fan"
myId = "was ist los"

testParser = print [html|<div class="test" id="{myId}">{{ test }}hi geht heute {{ wasser }}</div>|]