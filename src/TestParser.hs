{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module TestParser where
import Parser

test = "Wassermelone"

myId = "was ist los2"
test2 = ["Wassermelone", "salamander"]

wasser = ["4", "7"]

--testParser = [html|<div class="test" id="{myId}">{{ myId }} hi geht heute {{ myId }}</div>|]
--testParser = [html|<div [a<-wasser]>Hello World</div>|]
--testParser = [html|<div><span [a<-wasser]>{{test}} Hello World {{test}}</span></div>|]

f a = a