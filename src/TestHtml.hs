{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module TestHtml where
import Html
import HelperData

test = "True"

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


myVar3 = 3
myVar4 = 3

myVarA = "A"
myVarB = "A"


myVar5 = IntegerWrapper 1
myVar6 = IntegerWrapper 2

myArray = Wrapper_L[Wrapper_L[IntegerWrapper 1, IntegerWrapper 2], Wrapper_L[IntegerWrapper 1, IntegerWrapper 2]]



testHtml = [html|<div [a<-myArray]><span [b<-a]>Hello World</span></div>|]
--testHtml = [html|<div class="hello" hIf="myVar5 < myVar6"><span>Hello World</span></div>|]


--testParser = [html|<div class="test" id="{myId}">{{ myId }} hi geht heute {{ myId }}</div>|]
--testHtml = [html|<div [a<-wasser]><span [b<-a]>Hello World</span></div>|]
--testHtml = [html|<div [a <- melone]><span hIf="a < myVar4">Hello World</span></div>|]
--testHtml = [html|<div class="hello" hIf="myVarA == myVarB"><span hIf="myVarA == myVarB">{{ test }} Hello World</span></div>|]
--testHtml = [html|<div class="hello" hIf="myVar3 == myVar4"><span hIf="myVarA == myVarB">{{ test }} Hello World</span></div>|]
--testHtml = [html|<div class="hello" hIf="myVar3 == myVar4"><span>{{ test }} Hello World</span></div>|]
--testHtml = [html|<div class="hello">{{ test }} Hello World</div>|]
--testHtml = [html|<div hIf="myVar3 > myVar4 AND z">Hello World</div>|]
--testParser = [html|<div><span [a<-wasser]>{{test}} Hello World {{test}}</span></div>|]

f a = a

