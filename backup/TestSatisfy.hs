{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module TestSatisfy where
import Satisfy

test = "Wassermelone"

myId = "was ist los2"
test2 = ["Wassermelone", "salamander"]

--wasser = ["4", "7"]
wasser = [[3,2], [4,7]]

a = True
b = True
c = True
d = False
e = False
--testParser = [html|<div class="test" id="{myId}">{{ myId }} hi geht heute {{ myId }}</div>|]
testSat = [sat|d AND e|]
testSatFunc = eval testSat
--testParser = [html|<div><span [a<-wasser]>{{test}} Hello World {{test}}</span></div>|]

f a = a