{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module TestParser where
import Parser

test = "Wassermelone"
wasser = "fan"
myId = "was ist los2"
test2 = ["Wassermelone", "salamander"]


--testParser = [html|<div class="test" id="{myId}">{{ myId }} hi geht heute {{ myId }}</div>|]
testParser = [html|<div>Hello World</div>|]