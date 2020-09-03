{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module TestSimple2 where
import Simple2
import Language.Haskell.Interpreter

welt = "wasser"
hallo = "{ welt }"
--testSimple = print [simple|Hi[a<-wasser]|]

testSimple2 = [simple2|$hallo|]

