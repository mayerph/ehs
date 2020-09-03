{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module TestBool where
import Bool

test = "Wassermelone"

myId = "was ist los2"
test2 = ["Wassermelone", "salamander"]

--wasser = ["4", "7"]
wasser = [[3,2], [4,7]]

myVar1 = 1
myVar2 = 2

testBool = [bool|myVar1 gt myVar2|]

testBoolFunc = evalBool testBool
--testParser = [html|<div [a<-wasser]><span [b<-wasser]>Hello World</span></div>|]
--testParser = [html|<div><span [a<-wasser]>{{test}} Hello World {{test}}</span></div>|]

f a = a

