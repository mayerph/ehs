{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module TestParser where
import Parser

test = "Wassermelone"

myId = "was ist los2"
test2 = ["Wassermelone", "salamander"]

wasser = ["Wassermelone", "Pfirsich"]

--testParser = [html|<div class="test" id="{myId}">{{ myId }} hi geht heute {{ myId }}</div>|]
testParser = [html|<div [a<-wasser]>Hello World</div>|]