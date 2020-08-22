{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module TestSimple where
import Simple

test = ["Wassermelone", "Pfirsich"]
wasser = "fan"
wasser2 = [1, 2]
a1 = 1
a2 = 3
b = [1,2 :: Int]
testSimple = print [simple|Hi[wasser]|]

