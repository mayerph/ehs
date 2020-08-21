{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module TestSimple where
import Simple

test = "Wassermelone"
testSimple = print [simple|Hi{test}|]

