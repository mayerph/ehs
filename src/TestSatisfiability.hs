{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module TestSatisfiability where
import Satisfiability

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


myVar3 = 1
myVar4 = 2
--testParser = [html|<div class="test" id="{myId}">{{ myId }} hi geht heute {{ myId }}</div>|]
testSatis = [satis|myVar3 > myVar4 AND myVar3 > myVar4 |]
--testSatisFunc = eval testSatis
--testParser = [html|<div><span [a<-wasser]>{{test}} Hello World {{test}}</span></div>|]

f a = a