{-# LANGUAGE TemplateHaskell, QuasiQuotes #-} 
{-# Language FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module TestSimple where

import Simple;


testWeg = "hello"


data Foo = Foo {iS :: Int, sS :: String} 
    deriving (Show)

fSimple = Foo {sS = "foo1", iS = 1}

moreTest :: String -> String
moreTest a = a ++ "Welt"

testSimple :: SomeValue
testSimple = [simple|{{ T_String sS fSimple }}|]




