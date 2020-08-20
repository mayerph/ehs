{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module TH where

import Language.Haskell.TH 
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax 
import Control.Monad

doc :: QuasiQuoter
doc = QuasiQuoter
    htmlExpr
    undefined
    undefined
    undefined

htmlExpr :: String -> Q Exp 
htmlExpr = do
    x <- parseFromFile htmlContent "data.html"
    case x of 
        Right (a) -> do
        print a