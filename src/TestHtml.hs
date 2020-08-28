{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module TestHtml where
import Html

test = "Wassermelone"

myId = "1"
test2 = ["Wassermelone", "salamander"]

--wasser = ["4", "7"]
wasser = [[3,2], [4,7]]
melone = [1, 5]

--a = True
--b = True
z = True
c = True
d = False
e = False


myVar3 = 1
myVar4 = 3

--testParser = [html|<div class="test" id="{myId}">{{ myId }} hi geht heute {{ myId }}</div>|]
--testHtml = [html|<div [a<-wasser]><span [b<-wasser]>Hello World</span></div>|]
--testHtml = [html|<div [a <- melone]><span hIf="a > myVar4">Hello World</span></div>|]
testHtml = [html|<div class="hello" hIf="myVar4 > myVar3">Hello World</div>|]
--testHtml = [html|<div hIf="myVar3 > myVar4 AND z">Hello World</div>|]
--testParser = [html|<div><span [a<-wasser]>{{test}} Hello World {{test}}</span></div>|]

f a = a