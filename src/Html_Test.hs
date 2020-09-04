{-# LANGUAGE TemplateHaskell, QuasiQuotes #-} 
{-# Language FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Html_Test where
import Html_Parser
import Html_Data
import Data

test = "True"
test4 = T_String "hey"
myId = T_Int 1
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


myVar5 = T_Int 1
myVar6 = T_Int 0

myArray = T_List[T_List[T_Int 1, T_Int 2], T_List[T_Int 6, T_Int 8]]

gerta = T_Int 2
frieda = T_Int 7

testHtml :: [HTMLValue a]
testHtml = [html|<div>hello world</div>|]
--testHtml = [html|<div [a<-myArray]><span [b<-a]><div hIf="b > gerta">{{ b }} Hello World</div></span></div>|]
--testHtml = [html|<div class="hello" hIf="myVar5 > myVar6"><span>Hello World</span></div>|]


--testParser = [html|<div class="test" id="{myId}">{{ myId }} hi geht heute {{ myId }}</div>|]
--testHtml = [html|<div [a<-wasser]><span [b<-a]>Hello World</span></div>|]
--testHtml = [html|<div [a <- melone]><span hIf="a < myVar4">Hello World</span></div>|]
--testHtml = [html|<div class="hello" hIf="myVarA == myVarB"><span hIf="myVarA == myVarB">{{ test }} Hello World</span></div>|]
--testHtml = [html|<div class="hello" hIf="myVar3 == myVar4"><span hIf="myVarA == myVarB">{{ test }} Hello World</span></div>|]
--testHtml = [html|<div class="hello" hIf="myVar3 == myVar4"><span>{{ test }} Hello World</span></div>|]
--testHtml = [html|<div class="hello">{{ test }} Hello World</div>|]
--testHtml = [html|<div hIf="myVar3 > myVar4 AND z">Hello World</div>|]
--testParser = [html|<div><span [a<-wasser]>{{test}} Hello World {{test}}</span></div>|]


hello :: IO ()
hello = undefined

testHtml2 :: String
testHtml2 = show [html|<div>hello world</div>|]