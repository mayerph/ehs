{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module TestSimple where
import Simple

test = "Wassermelone"
wasser = "fan"
testSimple = print [simple|Hi{test}{wasser}|]

