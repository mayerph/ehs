{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module TestHtml where
import Html

test = "Wassermelone"

myId = "1"
test2 = ["Wassermelone", "salamander"]

--wasser = ["4", "7"]
wasser = [[3,2], [4,7]]

--testParser = [html|<div class="test" id="{myId}">{{ myId }} hi geht heute {{ myId }}</div>|]
--testHtml = [html|<div [a<-wasser]><span [b<-wasser]>Hello World</span></div>|]
testHtml = [html|<div>Hello World {{ myId }}</div>|]
--testParser = [html|<div><span [a<-wasser]>{{test}} Hello World {{test}}</span></div>|]

f a = a