{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module TestSimple where
import Simple

test = ["Wassermelone", "Pfirsich"]
wasser = ["Wassermelone", "Pfirsich"]
a1 = 1
a2 = 3
x = [1,2 :: Int]


testSimple = print [simple|Hi[wasser]|]

